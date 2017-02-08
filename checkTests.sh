
color="\033[44m"
endline="\033[0m\n----------\n\n"

cabal run samples/ambiguous1
echo  $color "ambiguous1   =====>  B=Unprovable" $endline

cabal run samples/easy
echo  $color "easy   =====>  B=True" $endline

cabal run samples/error1
echo  $color "error1   =====>  Incoherent" $endline

cabal run samples/error2
echo  $color "error2   =====>  C=False" $endline

cabal run samples/invalid
echo  $color "invalid   =====>  Incoherent" $endline

cabal run samples/invalid
echo  $color "invalid   =====>  Incoherent" $endline

cabal run samples/subject
echo  $color "subject   =====>  Incoherent" $endline

cabal run samples/subjectWithoutIncoherence
echo  $color "subjectWithoutIncoherence   =====>  Incoherent" $endline

cabal run samples/test1
echo  $color "test1   =====>  B=Unprovable D=Unprovable" $endline

cabal run samples/testAnonA
echo  $color "testAnonA   =====>  Enfaitcestfaux = False" $endline

cabal run samples/transposition
echo  $color "transposition   =====>  R = True" $endline

cabal run samples/unso
echo  "$color unso   =====>  Reponsefaux = False" $endline
