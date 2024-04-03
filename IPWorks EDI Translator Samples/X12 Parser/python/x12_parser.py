# 
# IPWorks EDI Translator 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks EDI Translator in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworkseditranslator
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworkseditranslator import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

import sys
import string
from ipworkseditranslator import *

input = sys.hexversion<0x03000000 and raw_input or input

## Define events!
def endFunctionalGroup(e):
  print("EndFunctionalGroup: " + e.tag)

def endInterchange(e):
  print("EndInterchange: " + e.tag)

def endLoop(e):
  print("EndLoop")

def endTransaction(e):
  print("EndTransaction: " + e.tag)

def error(e):
  print("ERROR: " + e.error_code + ':' + e.description)

def resolveSchema(e):
  print("ResolveSchema: " + e.transaction_code)

def segment(e):
  print("Segment: " + e.name)

def startFunctionalGroup(e):
  print("StartFunctionalGroup: " + e.tag)

def startInterchange(e):
  print("StartInterchange: " + e.tag)

def startLoop(e):
  print("StartLoop: " + e.name)

def startTransaction(e):
  print("StartTransaction: " + e.tag)

def warning(e):
  print("WARNING: " + e.warn_code + ": " + e.message)


## Check Command Line Args
if len(sys.argv) != 2:
  print("usage: x12parser file")
  print("")
  print("  file the x12 file to parse")
  print("\r\nExample: py x12parser.py ./x12.txt")
  sys.exit()

## Set up reader
x12reader1 = X12Reader();

## Add try to catch errors
try:
  ## Add events to reader
  x12reader1.on_end_functional_group = endFunctionalGroup
  x12reader1.on_end_interchange = endInterchange
  x12reader1.on_end_loop = endLoop
  x12reader1.on_end_transaction = endTransaction
  x12reader1.on_error = error
  x12reader1.on_resolve_schema = resolveSchema
  x12reader1.on_segment = segment
  x12reader1.on_start_functional_group = startFunctionalGroup
  x12reader1.on_start_interchange = startInterchange
  x12reader1.on_start_loop = startLoop
  x12reader1.on_start_transaction = startTransaction
  x12reader1.on_warning = warning

  ## Set up encoding and schema
  x12reader1.config("Encoding=iso-8859-1")
  x12reader1.set_schema_format(6);
  x12reader1.load_schema("./RSSBus_00401_810.json");

  ## Load in the input file and parse()
  x12reader1.set_input_file(sys.argv[1])
  x12reader1.parse()

except Exception as e:
  print(e)













