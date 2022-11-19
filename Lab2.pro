NOWARNINGS
DOMAINS
 file = datafile
 real_list = real*
 real_matrix = real_list*
PREDICATES
	readList(real_list,symbol,integer)
	readMatrix(real_matrix,integer,integer)
	isMinus(real,symbol,symbol,symbol,symbol)
	same(integer,integer)
	moreThanZero(real,symbol)
	moreThanZero(integer,symbol)
	readInputValues(integer,real,integer,real_list,real_list,real_matrix)
	checkIfValuesMoreThanZero(real,integer)

	checkIfMainDiagonalHasZeros(real_matrix,integer)
	normalizeMatrix(real_matrix,real_list,real_matrix,integer,real_matrix)
	findElementInArray(real_list,real,integer,integer)
	findElementInArray1(real_list,real,integer,integer,real_list)
	divideElements(real_list,real,real,real_list)
	
	checkIfDiagonalDominationFulfilled(real_matrix)
	sumArray(real_matrix,real_list)
	sum(real_list,real)
	findAllDiagonal(real_matrix,real_list,integer)
	sumArrayMinusDiagonal(real_list,real_list,real_list)
	checkIfDiagonalDomination(real_list,real_list)
	
	zeidel(real_list,real_matrix,real,integer,integer)
	calculateNewXs(real_list,real_matrix,real_list)
	calculateNewX(real_list,real_list,real)
	checkIfAccuracyReached(real_list,real_list,real)
	calculateAnswersAccuracy2(real_list,real,real_list)
	calculateAnswersAccuracy1(real_list,real_list,real_list)

CLAUSES


	%		LOAD SECTION

	
	isMinus(NEWH,CHECKSYMBOL,OLDH,OLDREST,NEWREST):- 
				NEWREST = OLDREST,str_real(OLDH,NEWH).
	isMinus(NEWH,OLDH,OLDH,OLDREST,NEWREST):- 
				fronttoken(OLDREST,STRINGH,NEWREST),
				str_real(STRINGH,REALH),
				NEWH = -1* REALH.
	
	readList([],"",COUNT):- 
				COUNT = 0,!.
	readList([H | T],LINE,COUNT):- 	
				fronttoken(LINE,H1,Rest),
				isMinus(H,"-",H1,Rest,NEWREST),
				readList(T,NEWREST,COUNT1),
				COUNT = COUNT1+1.
							
	readMatrix([],CHECK,COUNT):-
				eof(datafile),COUNT = 0,!. 		 
	readMatrix([H | T],CHECK,COUNT):-	
				not (eof(datafile)), 
				readln(LINE),
				readList(H,LINE,COUNT1),
				same(CHECK,COUNT1),
				readMatrix(T,CHECK,COUNT2),
				COUNT = COUNT2+1.
	
				
	same(NUM,NUM):-!.
	same(NUM1,NUM2):- 
				write("Error, check input file, Closing program"),nl,readchar(Temp_Exit),exit(1).
	
	moreThanZero(Element,ErrorString):-
				Element <= 0,!,
				write(ErrorString," , Closing program"),nl,readchar(Temp_Exit),exit(1);
				write("").
	
	readInputValues(MatrixSize,Epsilon,MaxIterations,X0s,MatrixAnswers,Matrix):-
				readint(MatrixSize),
				readreal(PreEpsilon),
				Epsilon = abs(PreEpsilon),
				readint(PreMaxIterations),
				MaxIterations = abs(PreMaxIterations),
				
				readln(X0sSymbol),
				readList(X0s,X0sSymbol,X0sCount),
				same(X0sCount,MatrixSize),
				
				readln(MatrixAnswersSymbol),
				readList(MatrixAnswers,MatrixAnswersSymbol,MatrixAnswersCount),
				same(MatrixAnswersCount,MatrixSize),
				
				readMatrix(Matrix,MatrixSize,MatrixSizeCalculated),
				same(MatrixSize,MatrixSizeCalculated).
	
	checkIfValuesMoreThanZero(Epsilon,MaxIterations):-
				MaxIterations <= 0,
				write("Max Iterations amount can't be less than 1, closing program"),
				readchar(TEMPO),
				exit(1);
				Epsilon <=0,
				write("Epsilon can't be less or be equal to 0, closing program"),
				readchar(TEMP1),
				exit(1);
				write("").
	checkIfMainDiagonalHasZeros([],_).
	checkIfMainDiagonalHasZeros([HArray|TArrays],Index):-
				findElementInArray(HArray,Element,Index,1),
				Element = 0,
				write("Main diagonal of matrix can't contain zeros. Check input file, closing program"),
				readchar(TEMPO),
				exit(1);
				NewIndex = Index + 1,
				checkIfMainDiagonalHasZeros(TArrays,NewIndex).
				
	findElementInArray([H|T],H,Index,Index).
	findElementInArray([H|T],Answer,Index,CurrentIndex):-
				NewCurrentIndex = CurrentIndex + 1,
				findElementInArray(T,Answer,Index,NewCurrentIndex).
				
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	checkIfDiagonalDominationFulfilled(Matrix):-
				sumArray(Matrix,ArrayOfSumByLine), 
				findAllDiagonal(Matrix,ArrayOfDiagonal,1),
				sumArrayMinusDiagonal(ArrayOfSumByLine,ArrayOfDiagonal,ArrayReadyForChecking),
				checkIfDiagonalDomination(ArrayReadyForChecking,ArrayOfDiagonal).
				
	sumArray([],[]).
	sumArray([Line|AnotherLines],[Answer|Answers]):-
				sum(Line,Answer),
				sumArray(AnotherLines,Answers).
				
	sum([H],H).
	sum([H|T],Answer):-
				sum(T,LocalAnswer),
				Answer = LocalAnswer + H.
	
	findAllDiagonal([],[],_).
	findAllDiagonal([Line|AnotherLines],[Answer|Answers],Index):-
				findElementInArray(Line,Answer,Index,1),
				NewIndex = Index + 1,
				findAllDiagonal(AnotherLines,Answers,NewIndex).
	
	sumArrayMinusDiagonal([],[],[]).
	sumArrayMinusDiagonal([H1|T1],[H2|T2],[H3|T3]):-
				Temp = abs(H2),
				H3 = H1 - Temp,
				sumArrayMinusDiagonal(T1,T2,T3).
				
	checkIfDiagonalDomination([],[]).
	checkIfDiagonalDomination([LineSum|AnotherLinesSum],[Diagonal|Diagonals]):-
				Diagonal < LineSum,
				write("Diagonal Domination is not fulfilled, method discrepancy occured! Closing program"),nl,
				readchar(_),
				exit;
				checkIfDiagonalDomination(AnotherLinesSum,Diagonals).
	

	%		PREPARATIONS STAGE

	normalizeMatrix([],[],[],_,_).
	normalizeMatrix([HArray|TArrays],[HMatrixAnswers|TMatrixAnswers],[HAnswer|TAnswer],Index,[AnswerArray|AnswerTail]):-
				findElementInArray1(HArray,XElement,Index,1,AnswerArray),
				divideElements(AnswerArray,XElement,HMatrixAnswers,HAnswer),
				NewIndex = Index + 1,
				normalizeMatrix(TArrays,TMatrixAnswers,TAnswer,NewIndex,AnswerTail).
				
	findElementInArray1([H|T],H,Index,Index,[0|T]).
	findElementInArray1([H|T],Answer,Index,CurrentIndex,[H|T1]):-
				NewCurrentIndex = CurrentIndex + 1,
				findElementInArray1(T,Answer,Index,NewCurrentIndex,T1).
	
	divideElements([],DivisionElement,EndNumber,[HAnswer]):-
				HAnswer = EndNumber/DivisionElement.
	divideElements([H|T],DivisionElement,EndNumber,[HAnswer|TAnswer]):-
				HAnswer = -1 * (H/DivisionElement),
				divideElements(T,DivisionElement,EndNumber,TAnswer).
	
	%
	%		 ZEIDEL METHOD
	%

	zeidel(Xs,Matrix,Epsilon,MaxIterations,MaxIterations):-
				write("Max iteration reached. Last result: ",Xs," Closing program..."),
				readchar(TEMP),
				exit(1).
	zeidel(Xs,Matrix,Epsilon,MaxIterations,CurrentIteration):-
				calculateNewXs(Xs,Matrix,NewXs),
				checkIfAccuracyReached(Xs,NewXs,Epsilon),
				NewCurrentIteration = CurrentIteration + 1,
				zeidel(NewXs,Matrix,Epsilon,MaxIterations,NewCurrentIteration).
	calculateNewXs(_,[],[]).
	calculateNewXs(Xs,[HeadArray|TArrays],[HAnswer|TAnswers]):-%(Previous X'es,Matrix Line, New X'es, Index of)
				calculateNewX(Xs,HeadArray,HAnswer),
				calculateNewXs(Xs,TArrays,TAnswers).

	calculateNewX([],[H],H).%3 4 _
	calculateNewX([HXs|TXs],[H|T],Answer):- 
				calculateNewX(TXs,T,LocalAnswer),
				Answer = LocalAnswer + HXs * H.

	%%%%%%%%%%%%%%%%%%%%%%%

	checkIfAccuracyReached(Xs,NewXs,Epsilon):-
				calculateAnswersAccuracy1(Xs,NewXs,XsAccuracy),
				calculateAnswersAccuracy2(XsAccuracy,Epsilon,NewXs).
	
	calculateAnswersAccuracy1([],[],[]).	
	calculateAnswersAccuracy1([H1|T1],[H2|T2],[H3|T3]):-	
				TEMP = H1 - H2,
				H3 = abs(TEMP),
				calculateAnswersAccuracy1(T1,T2,T3).
				
	calculateAnswersAccuracy2([],Epsilon,NewXs):-
				write("Accuracy reached. Answer is ",NewXs," Closing program."),
				readchar(TEMP0),
				exit(1).
	calculateAnswersAccuracy2([H|T],Epsilon,NewXs):-
				Epsilon > H,
				calculateAnswersAccuracy2(T,Epsilon,NewXs);
				write("").
GOAL
	makewindow(1, 6,6, "Lab2", 1, 0, 24, 80),
	%LOAD SECTION
	openread(datafile,"file.txt"),
	readdevice(datafile),
	readInputValues(MatrixSize,Epsilon,MaxIterations,X0s,MatrixAnswers,Matrix),
	readdevice(stdin),
	closefile(datafile),
	%MISC
	write("Matrix Size: ",MatrixSize),nl,
	write("Epsilon: ",Epsilon),nl,
	write("Max Iterations: ",MaxIterations),nl,
	write("X0s: ",X0s),nl,
	write("Right part: ",MatrixAnswers),nl,
	write("Input matrix: ",Matrix),nl,
	write("================================="),nl,
	%PREPS
	checkIfValuesMoreThanZero(Epsilon,MaxIterations),
	checkIfMainDiagonalHasZeros(Matrix,1),
	checkIfDiagonalDominationFulfilled(Matrix),
	normalizeMatrix(Matrix,MatrixAnswers,PreparedMatrix,1,_),
	%write("Prepared matrix: ",PreparedMatrix),nl,
	%ZEIDEL
	zeidel(X0s,PreparedMatrix,Epsilon,MaxIterations,1),
	readchar(TEMPO).

	
	