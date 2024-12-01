ROOT_INCLUDE := `root-config --incdir`
ROOT_LIBS := `root-config --libs`
ROOT_CFLAGS := `root-config --cflags`

PROG := plot

compile: main.cpp
	g++ -${ROOT_CFLAGS} -I${ROOT_INCLUDE} $^ -o ${PROG} ${ROOT_LIBS}

clean:
	-rm ${PROG}
