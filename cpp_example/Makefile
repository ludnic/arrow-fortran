
CXX = g++ 
CXXFLAGS = -g -Wall -Wextra $(shell pkg-config --cflags arrow)
LDFLAGS = $(shell pkg-config --libs arrow)

../bin/ArrowTest: main.cc
	$(CXX) -o $@ $(CXXFLAGS) $< $(LDFLAGS)