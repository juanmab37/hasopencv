.PHONY : clean

CVINPUT=-I /usr/local/include/
CVFLAGS=-L/usr/local/lib -lopencv_core
CVCOMPILE=`pkg-config opencv --cflags --libs`
DIR= HOpenCV/CV

all : prog

clean :
	rm -f $(DIR)/*.o
	rm -f $(DIR)/*.gch
	rm -f $(DIR)/*.hi

objects = $(DIR)/HOpenCV_wrap.o $(DIR)/CxCore.o $(DIR)/HighGui.o $(DIR)/CV.o

prog: $(objects) 
	ghc --make main.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE) -o prog
        
$(DIR)/%.hs: $(DIR)/%.hsc 
	hsc2hs -c g++ $< $(CVINPUT)

$(DIR)/CxCore.o: $(DIR)/CxCore.hs 
	ghc --make $(DIR)/CxCore.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE)

$(DIR)/CV.o: $(DIR)/CV.hs 
	ghc --make $(DIR)/CV.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE)

$(DIR)/HighGui.o: $(DIR)/HighGui.hs 
	ghc --make $(DIR)/HighGui.hs $(DIR)/HOpenCV_wrap.cpp $(CVCOMPILE)


