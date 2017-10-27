import algorithm
import strutils
import sequtils
import json
import tables
import os


# Assembly

let maxPadding = {
  "mem": 13,
  "cmd": 3,
  "arg": 10
}.toTable
let special = {
  "adr": "$",
  "D8": "#0x",
  "D16": "#0x"
}.toTable

var outfileName: string = "0_out.s"
var outfile: File

proc updateOutfile() =
  if fileExists("asm/" & outfileName) and
     getFileSize("asm/" & outfileName) <= 10240:
    return
  var indexStr = outfileName.split("_")[0]
  var
    index: int = parseInt(indexStr) + 1
    name = outfileName[indexStr.len + 1..^0]
  outfileName = $index & "_" & name
  try:
    outfile = open("asm/" & outfileName, fmAppend)
  except IOError:
    return


proc readJson(filename: string): JsonNode =
  let jsonString: string = readFile(filename)
  return parseJson(jsonString)


proc padding(already, mode: string): string =
  var padding: string = " "
  for i in 0..(maxPadding[mode] - already.len):
    padding &= " "
  return already & padding


type Translator = ref object
  # general state
  binSeq: seq[uint8]
  hexSeq: seq[string]
  databook: JsonNode
  filename: string
  # instruction state
  instruction: string
  function: string
  size: int
  origSize: int
  badop: bool
  # op state
  adr: string
  hex: string
  hexParams: string
  command: string
  params: seq[string]

method bytes(self: Translator) {.base.} =
  let binFile: File = open(self.filename)
  let length: int64 = getFileSize(binFile)
  self.binSeq = newSeq[uint8](length)
  discard readBytes(binFile, self.binSeq, 0, length)
  close(binFile)

method hexdump(self: Translator) {.base.} =
  self.bytes()
  self.hexSeq = map[uint8, string](self.binSeq, proc(x: uint8): string =
    toHex(x).toLowerAscii)

method parseInstruction(self: Translator) {.base.} =
  var i: seq[string] = splitWhitespace(self.instruction)
  var p: seq[string] = @[]
  if i.len > 1:
    p = map(split(i.pop(), sep=','), toLowerAscii)
  self.command = i.pop().toLowerAscii
  self.params = p

method nextop(self: Translator) {.base.} =
    let op: JsonNode = self.databook["0x" & self.hex]
    if op["instruction"].getStr() == "-":
      self.badop = true
      return
    self.instruction = op["instruction"].getStr()
    self.function = op["function"].getStr()
    self.size = parseInt(op["size"].getStr())
    self.origSize = self.size
    self.badop = false

method grab(self: Translator, param: string, start: int): string {.base.} =
    var newParam: string = special[param]
    for i in countdown(self.size, 1):
      newParam &= self.hexSeq[start + i]
      dec(self.size)
    return newParam

method stretch(self: Translator, start: int) {.base.} =
    for i, param in self.params:
      if special.hasKey(param):
        self.params[i] = self.grab(param, start)

method prettify(self: Translator) {.base.} =
  var mem: string = self.adr & " " & self.hex & " " & self.hexParams
  var params = join(self.params, sep=",").toLowerAscii
  var comment = "(" & $self.origSize & ") " & self.function.toLowerAscii
  write(outfile, padding(mem, "mem") &
                 padding(self.command, "cmd") &
                 padding(params, "arg") &
                 " ; " & comment & "\n")

method translate(self: Translator) {.base.} =
  var i: int = 0
  while i < self.hexSeq.len:
    updateOutfile()
    self.hex = self.hexSeq[i]
    self.nextop()
    if self.badop:
      inc(i)
      continue
    self.parseInstruction()
    dec(self.size)
    self.hexParams = ""
    for i_adr in 1..self.size:
      self.hexParams &= self.hexSeq[i + i_adr] & " "
    self.stretch(i)
    self.adr = toHex(i, 4).toLowerAscii
    self.prettify()
    inc(i, self.origSize)

proc newTranslator(filename: string): Translator =
  var t: Translator = Translator()
  t.filename = filename
  t.databook = readJson("assets/8080-databook.json")
  t.hexdump()
  return t


# Emulation

type ConditionCodes = ref object
  z: uint8
  s: uint8
  p: uint8
  cy: uint8
  ac: uint8
  pad: uint8

proc newConditionCodes(): ConditionCodes =
  return ConditionCodes(z: 1, s: 1, p: 1, cy: 1, ac: 1, pad: 3)


type State8080 = ref object
  a: uint8
  b: uint8
  c: uint8
  d: uint8
  e: uint8
  h: uint8
  l: uint8
  sp: uint16
  pc: int
  memory: ref uint8
  cc: ConditionCodes
  int_enable: uint8
  binSeq: seq[uint8]

method unimplemented(self: State8080, opcode: uint8) {.base.} =
  return
  # echo "error: unimplemented instruction '", opcode, "'"
  # quit(1)

method noop(self: State8080) {.base.} =
  return

method accumulate(self: State8080, register: uint8): uint16 {.base.} =
  return (uint16)(self.a) + (uint16)(register)


method evalFlags(self: State8080, acc: uint16) {.base.} =
  self.cc.z = (uint8)((acc and 0xff) == (uint16)0)
  self.cc.s = (uint8)((acc and 0x80) != (uint16)0)
  self.cc.cy = (uint8)(acc > (uint16)0xff)
  # self.cc.p = parity(acc and 0xff)
  self.a = acc and 0xff

method emulate(self: State8080) {.base.} =
  var opcode: uint8 = self.binSeq[self.pc]
  case opcode
  of 0x00: self.noop()
  of 0x01: # LXI B,D16
    self.c = self.binSeq[self.pc+1]
    self.b = self.binSeq[self.pc+2]
    self.pc += 2
  of 0x41: # MOV B,C
    self.b = self.c
  of 0x42: # MOV B,D
    self.b = self.d
  of 0x43: # MOV B,E
    self.b = self.e
  of 0x80: # ADD B
    var acc = self.accumulate(self.b)
    self.evalFlags(acc)
  of 0x81: # ADD C
    var acc = self.accumulate(self.c)
    self.evalFlags(acc)
  of 0xc6: # ADI byte
    var acc = self.accumulate(self.binSeq[self.pc+1])
    self.evalFlags(acc)
  of 0x86: # ADD M
    var offset: uint16 = (self.h shl 8) or self.l
    var acc: uint16 = (uint16) self.a + self.binSeq[(int)offset]
    self.evalFlags(acc)
  of 0xc2: # JNZ address
    if self.cc.z == 0:
      self.pc = (int)((self.binSeq[self.pc+2] shl 8) or self.binSeq[self.pc+1])
    else:
      self.pc += 2
  of 0xc3: # JMP address
    self.pc = (int)((self.binSeq[self.pc+2] shl 8) or self.binSeq[self.pc+1])
  of 0xcd: # CALL address
    var ret: uint16 = (uint16)(self.pc + 2)
    self.binSeq[self.pc-1] = (ret shr 8) and 0xff
    self.binSeq[self.pc-2] = ret and 0xff
    self.sp = self.sp - 2
    self.pc = (int)((self.binSeq[self.pc+2] shl 8) or self.binSeq[self.pc+1])
  of 0xc9: # RET
    self.pc = (int)(self.binSeq[(int)self.sp] or (self.binSeq[(int)self.sp+1] shl 8))
    self.sp += 2
  else:
    self.unimplemented(opcode)
  self.pc += 1

proc newState8080(): State8080 =
  var t: Translator = newTranslator("assets/mario.nes")
  var s: State8080
  t.translate()
  s = State8080(binSeq: t.binSeq, cc: newConditionCodes())
  while true:
    s.emulate()
  return s

discard newState8080()
