#
# IPWorks EDI Translator 2024 .NET Edition - Sample Project
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

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lipworkseditranslator.24.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o edifactwriter edifactwriter.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/ipworkseditranslator.o
	g++ $(OS_CFLAGS) -o edifactwriter ../../src/ipworkseditranslator.o edifactwriter.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/ipworkseditranslator.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/ipworkseditranslator.o ../../src/ipworkseditranslator.cpp

else # LINUX
ifeq ($(shell uname -m), x86_64)
  LIB=lib64
else
  LIB=lib
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lipworkseditranslator  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX -o edifactwriter edifactwriter.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH) 

src: ../../src/ipworkseditranslator.o
	g++ -D UNIX -o edifactwriter ../../src/ipworkseditranslator.o edifactwriter.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/ipworkseditranslator.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/ipworkseditranslator.o ../../src/ipworkseditranslator.cpp

clean:
	rm -f ../../src/*.o
	rm -f edifactwriter *.o
endif