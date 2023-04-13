
CXX = g++ 
CXXFLAGS = -g -Wall -Wextra $(shell pkg-config --cflags arrow)
LDFLAGS = $(shell pkg-config --libs arrow parquet)

all: ../bin/arrow_data_structures_cpp ../bin/arrow_io_cpp

../bin/arrow_data_structures_cpp: arrow_data_structures.cc
	$(CXX) -o $@ $(CXXFLAGS) $< $(LDFLAGS)

../bin/arrow_io_cpp: arrow_io.cc
	$(CXX) -o $@ $(CXXFLAGS) $< $(LDFLAGS)