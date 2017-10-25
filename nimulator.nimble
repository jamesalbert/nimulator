# Package

version       = "0.0.1"
author        = "James Albert"
description   = "8080 emulator"
license       = "MIT"
bin           = @["nimulator"]

# Dependencies

requires "nim >= 0.17.2"

task clean, "cleaning project":
  exec "rm -rf asm nimulator nimcache"

task run, "running project":
  exec "nimble clean"
  exec "nimble build"
  exec "mkdir asm"
  exec "./nimulator"
