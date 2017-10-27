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

method dump(self: State8080) {.base.} =
  echo "==Nimulator Dump=="
  echo "values are formatted as dec / hex / bin"
  echo "@@@ registers"
  echo "a: ", self.a, " / ", toHex((int)self.a, 4), " / ", toBin((int)self.a, 8)
  echo "b: ", self.b, " / ", toHex((int)self.b, 4), " / ", toBin((int)self.b, 8)
  echo "c: ", self.c, " / ", toHex((int)self.c, 4), " / ", toBin((int)self.c, 8)
  echo "d: ", self.d, " / ", toHex((int)self.d, 4), " / ", toBin((int)self.d, 8)
  echo "e: ", self.e, " / ", toHex((int)self.e, 4), " / ", toBin((int)self.e, 8)
  echo "h: ", self.h, " / ", toHex((int)self.h, 4), " / ", toBin((int)self.h, 8)
  echo "l: ", self.l, " / ", toHex((int)self.l, 4), " / ", toBin((int)self.l, 8)
  echo "@@@ memory"
  echo "sp: ", self.sp, " / ", toHex((int)self.sp, 4), " / ", toBin((int)self.sp, 8)
  echo "pc: ", self.pc, " / ", toHex((int)self.pc, 4), " / ", toBin((int)self.pc, 8)
  echo "memory: ", self.binSeq[self.pc], " / ", toHex((int)self.binSeq[self.pc], 4), " / ", toBin((int)self.binSeq[self.pc], 8)
  echo "int_enable: ", self.int_enable, " / ", toHex((int)self.int_enable, 4), " / ", toBin((int)self.int_enable, 8)
  echo "@@@ flags"
  echo "z: ", self.cc.z, " / ", toHex((int)self.cc.z, 4), " / ", toBin((int)self.cc.z, 8)
  echo "s: ", self.cc.s, " / ", toHex((int)self.cc.s, 4), " / ", toBin((int)self.cc.s, 8)
  echo "p: ", self.cc.p, " / ", toHex((int)self.cc.p, 4), " / ", toBin((int)self.cc.p, 8)
  echo "cy: ", self.cc.cy, " / ", toHex((int)self.cc.cy, 4), " / ", toBin((int)self.cc.cy, 8)
  echo "ac: ", self.cc.ac, " / ", toHex((int)self.cc.ac, 4), " / ", toBin((int)self.cc.ac, 8)
  echo "pad: ", self.cc.pad, " / ", toHex((int)self.cc.pad, 4), " / ", toBin((int)self.cc.pad, 8)

method noop(self: State8080) {.base.} =
  return

method add(self: State8080, regr: uint8, regl: uint8): uint16 {.base.} =
  return (uint16)(regr) + (uint16)(regl)

method accumulate16(self: State8080, acc: uint16) {.base.} =
  self.a = acc and 0xff

method accumulate8(self: State8080, acc: uint8) {.base.} =
  self.a = acc and 0xf

method evalFlags16(self: State8080, acc: uint16) {.base.} =
  self.cc.z = (uint8)((acc and 0xff) == 0'u16)
  self.cc.s = (uint8)((acc and 0x80) != 0'u16)
  self.cc.cy = (uint8)(acc > 0xff'u16)
  # self.cc.p = parity(acc and 0xff)

method evalFlags8(self: State8080, acc: uint8) {.base.} =
  self.cc.z = (uint8)((acc and 0xf) == 0'u8)
  self.cc.s = (uint8)((acc and 0x8) != 0'u8)
  self.cc.cy = (uint8)(acc > 0xf'u8)
  # self.cc.p = parity(acc and 0xff)

method emulate(self: State8080) {.base.} =
  var opcode: uint8 = self.binSeq[self.pc]
  case opcode
  of 0x00: self.noop()
  of 0x01: # LXI B,D16
    self.c = self.binSeq[self.pc+1]
    self.b = self.binSeq[self.pc+2]
    self.pc += 2
  of 0x04: # INR B; 1 byte
    self.b += 1'u8
    self.evalFlags8(self.b)
  of 0x05: # DCR B; 1 byte
    self.b -= 1'u8
    self.evalFlags8(self.b)
  of 0x06: # MVI B, D8; 2 bytes
    self.b = self.binSeq[self.pc+1]
    self.pc += 1
  of 0x07: # RLC; 1 byte
    # A = A << 1; bit 0 = prev bit 7; CY = prev bit 7
    self.a = self.a shl 1'u8
    # not finished
  of 0x41: # MOV B,C
    self.b = self.c
  of 0x42: # MOV B,D
    self.b = self.d
  of 0x43: # MOV B,E
    self.b = self.e
  of 0x80: # ADD B
    var acc = self.add(self.a, self.b)
    self.evalFlags16(acc)
    self.accumulate16(acc)
  of 0x81: # ADD C
    var acc = self.add(self.a, self.c)
    self.evalFlags16(acc)
    self.accumulate16(acc)
  of 0xc6: # ADI byte
    var acc = self.add(self.a, self.binSeq[self.pc+1])
    self.evalFlags16(acc)
    self.accumulate16(acc)
  of 0x86: # ADD M
    var offset: uint16 = (self.h shl 8) or self.l
    var acc: uint16 = (uint16) self.a + self.binSeq[(int)offset]
    self.evalFlags16(acc)
    self.accumulate16(acc)
  of 0xc2: # JNZ address
    if self.cc.z == 0:
      self.pc = (int)((self.binSeq[self.pc+2] shl 8) or self.binSeq[self.pc+1])
    else:
      self.pc += 2
  of 0xd2: # JNC address; 3 bytes
    if self.cc.cy == 0:
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
  while s.pc < s.binSeq.len:
    try:
      s.emulate()
    except:
      let
        e = getCurrentException()
        msg = getCurrentExceptionMsg()
      echo "Got exception ", repr(e), " with message ", msg
      s.dump()
      quit(0)
  return s

discard newState8080()
