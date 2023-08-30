del gm82save.dll

cargo build --release
upx --lzma target\i686-pc-windows-msvc\release\gm82save.dll -o gm82save.dll

pause