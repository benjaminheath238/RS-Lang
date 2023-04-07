NIM = nim

compile:
	$(NIM) c rslc.nim

clean:
	rm ./*.out
	rm ./*.c
	rm ./*.h
	rm ./rslc
