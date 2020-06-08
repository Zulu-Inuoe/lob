@setlocal

@set LOB_ASD=%~dp0src\lob.asd
@set LOB_ASD=%LOB_ASD:\=\\%
@set LOB_EXE=%~dp0bin\lob.exe
@set LOB_EXE=%LOB_EXE:\=\\%

@set BUILD_EXP=^
(if (lob:build :image \"sbcl\" :loaded-things \"%LOB_ASD%\" :output-path \"%LOB_EXE%\")^
  (sb-ext:exit :code 0)^
  (sb-ext:exit :code 1))

@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --eval "(require ""ASDF\"")"^
 --eval "(asdf:load-asd #p""%LOB_ASD%\"")"^
 --eval "(asdf:load-system '#:lob)"^
 --eval "%BUILD_EXP%"

@endlocal
