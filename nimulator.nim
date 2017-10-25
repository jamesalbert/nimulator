import algorithm
import strutils
import sequtils
import json
import tables
import os


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
    toHex(x).toLower)

method parseInstruction(self: Translator) {.base.} =
  var i: seq[string] = splitWhitespace(self.instruction)
  var p: seq[string] = @[]
  if i.len > 1:
    p = map(split(i.pop(), sep=','), toLower)
  self.command = i.pop().toLower()
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
  var params = join(self.params, sep=",").toLower()
  var comment = "(" & $self.origSize & ") " & self.function.toLower()
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
    self.adr = toHex(i, 4).toLower()
    self.prettify()
    inc(i, self.origSize)

proc newTranslator(filename: string): Translator =
  var t: Translator = Translator()
  t.filename = filename
  t.databook = readJson("assets/8080-databook.json")
  t.hexdump()
  return t


var t: Translator = newTranslator("assets/mario.nes")
t.translate()
