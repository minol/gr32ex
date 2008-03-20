@echo off
echo CLEANING UP...

cd "Basic SFDS File Creator"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "Load ImageText"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "MakeLinks"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "Search Test"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "SFDS Script"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "SFX win32"
del *.exe
del default_sfx.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "SimpleTest"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "TC Plugin"
del SFDS_TC.wcx
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "GenerateLotsOfMetadata"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd "MultiThreading"
del *.exe
del *.res
del *.identcache
del *.local
del __history\*.*
rmdir __history
cd ..

cd ".."
del __history\*.*
rmdir __history
del dcudump\*.*
cd demos