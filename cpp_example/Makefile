
CXX = g++ 
CXXFLAGS = -g -Wall -Wextra $(shell pkg-config --cflags arrow)
LDFLAGS = $(shell pkg-config --libs arrow)

../bin/arrow_data_structures_cpp: arrow_data_structures.cc
	$(CXX) -o $@ $(CXXFLAGS) $< $(LDFLAGS)