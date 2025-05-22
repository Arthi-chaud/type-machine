#! /bin/bash


for i in TM SR Extensible; do
	rm -f ./*.hi ./*.o TM SR Extensible
	echo $i 
	time stack ghc -- $i.hs 
done
