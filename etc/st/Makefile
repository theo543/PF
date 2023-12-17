.PHONY: all doubles_files benchmark 

all: doubles_files st

doubles_files: st_genDoubles doubles_large.txt doubles_small.txt

st_genDoubles: st_genDoubles.cpp
	g++ st_genDoubles.cpp -O3 -o $@

doubles_large.txt: st_genDoubles
	./st_genDoubles 5000000 > $@

doubles_small.txt: st_genDoubles
	./st_genDoubles 500000 > $@

st: st.hs
	ghc -O3 st.hs -o $@ -no-keep-hi-files -no-keep-o-files

define BENCHMARK_SCRIPT
while true; do
	printf "\nsmall test case, no lazy IO, pure\n"
	./st --pure < doubles_small.txt
	printf "\nsmall test case, no lazy IO, ST\n"
	./st --st < doubles_small.txt
	printf "\nlarge test case, lazy IO, pure\n"
	./st --lazy-io --pure < doubles_large.txt
	printf "\nlarge test case, lazy IO, ST\n"
	./st --lazy-io --st < doubles_large.txt
	read -p "Press enter to repeat or type 'exit' to exit: " userInput
	if [ "$$userInput" == "exit" ]; then
		break
	fi
done
endef

export BENCHMARK_SCRIPT

benchmark: doubles_files st
	bash -c "$$BENCHMARK_SCRIPT"