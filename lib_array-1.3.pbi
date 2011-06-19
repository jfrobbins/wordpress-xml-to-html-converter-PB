; AUTHOR:  Jon Robbins
; ================================================
; RFMD
; 7858 Thorndike Rd
; Greensboro, NC  27409         jrobbins@rfmd.com
; p: 336-678-5364               c: 336-314-7262
; ================================================

;============================================================================
;-Array functions
;=============================================================================
;-Version:
;-2010.06.10
EnableExplicit

XIncludeFile "lib_string-1.2.pbi"

;-
;-Copy Arrays
Procedure CopyArrayStr (Array StringsIn.s(1), Array StringsOut.s(1))
   Protected size, i
   
   size = ArraySize(StringsIn())
   If size <> ArraySize(StringsOut())
      ReDim StringsOut(size)
   EndIf
   
   For i = 0 To size
      StringsOut(i) = StringsIn(i)
   Next
EndProcedure

Procedure CopyArrayI (Array aIN.i(1), Array aOUT.i(1))
   Protected size
   
   size = ArraySize(aIn())
   ReDim aOut(size)
   CopyMemory(aIN(),aOUT(),(size+1) * SizeOf(Integer))   
EndProcedure

Procedure CopyArrayD (Array aIN.D(1), Array aOUT.D(1))
   Protected size
   
   size = ArraySize(aIn())
   ReDim aOut(size)
   CopyMemory(aIN(),aOUT(),(size+1) * SizeOf(Double))   
EndProcedure

;-
;- Explode/Implode
Procedure.i explodeArray(Array sArr.s(1),  string.s, separator.s = ",", trimString.i = #False, limit.i = 0) ; String to Array
  ;From Flype's Functions - http://www.purebasic.fr/english/viewtopic.php?f=12&t=21495&start=0
  ;takes a list of delimited string and explodes out into an array  
  Protected.i index, size = CountString(string, separator)
  If Len(separator) > 1
    ProcedureReturn 0
  EndIf

  If (limit > 0)
    size = limit - 1
  ElseIf (limit < 0)
    size + limit
  EndIf

  ReDim sArr.s(size)

  For index = 0 To size
    If trimString
      sArr(index) = Trim(StringField(string, index + 1, separator))
    Else
      sArr(index) = StringField(string, index + 1, separator)
    EndIf
  Next index

  ProcedureReturn size
EndProcedure

Procedure.i StoreFields (Array arr.s(1), sline.s, del.s = ",", limit.i = 0)
  ;explodes array and trims contents to remove whitespace
  ;  ie. "blah blah,blah,   blah" -> blah blah,blah,blah (but in array)
  Protected.i i, uB

  uB = explodeArray(arr(),sline,del)
  
  For i = 0 To uB
    arr(i) = Trim(arr(i))
  Next i
  ProcedureReturn uB
EndProcedure 

Procedure.s implodeArray(Array arr.s(1), separator.s = ",") ; Array to String
;http://www.purebasic.fr/english/viewtopic.php?f=12&t=21495&start=0
  Protected index.i, result.s, size.i = ArraySize(Arr())
  
  For index = 0 To size
    result + Arr(index)
    If (index < size)
      result + separator
    EndIf
  Next index
 
  ProcedureReturn result 
EndProcedure

;-
;- Array positions
Procedure.i ArrayPositionStr(Array arr.s(1), value.s, CaseSensitive.i = #False)
;   '=============================================================
;   'Searches array for value
;   ' Returns Location of value if value is in array. -1 if not.
;   '=============================================================  
  
  Protected.i i,ub
  
  ub=ArraySize(arr(),1)
  
  If Not CaseSensitive
    For i = 0 To ub Step 1
      If LCase(arr(i)) = LCase(value) 
        ProcedureReturn i
      EndIf
    Next i
  Else
    For i = 0 To ub Step 1
      If arr(i) = value 
        ProcedureReturn i
      EndIf
    Next i
  EndIf
  
  ;'if this is accessed, then end of array was reached without finding the value
  ProcedureReturn -1
EndProcedure 

;-
;-Array data entry
Procedure.i EnterDataInArrayStr(Array arr.s(1),value.s, count.i)
  Protected.i i
  
  i = ArrayPositionStr(arr(), value) 
  If i <0
    If count > ArraySize(arr())
      ReDim arr(count)
    EndIf       
    arr(count) = value
    count+1
  EndIf
  ProcedureReturn count
EndProcedure  

;-
;-Edit array contents
Procedure.i ar_1DcsvTo2D(Array aout.s(2), Array x.s(1), del.s = ",")
  ; Convert 1D CSV string Array To 2D
  Protected.i nc,nr
  
  Protected.i cUB = CountString(x(0),del)
  Protected.i rUB = ArraySize(x())
  
  Dim aout(cUB,rUB)
  
  For nr = 0 To rUB    
    For nc = 0 To cUB        
      aout(nc,nr) = StringField(x(nr),nc+1,del)
    Next nc
  Next nr
  
  ProcedureReturn rUB
EndProcedure

Procedure.i ar_ClearExcessEmptyRows(Array ar2d.s(2), RedimArrayInHere.i=#True)
  ;clears empty space from end of array. returns the last populated row if successful.
  ; re-dims the array to appropriate size.
  Protected.i nr,nc,rUB,cUB
  Protected.i FirstEmptyRow = -1
  Protected.i LastEmptyRow  = -1
  Protected.i RowIsEmpty
  
  cUB = ArraySize(ar2d(),1)
  rUB = ArraySize(ar2d(),2)
  
  For nr=0 To rUB Step 1    
    RowIsEmpty = #True 
    For nc=0 To cUB Step 1
      If ar2d(nc,nr) <> ""
        RowIsEmpty = #False
        FirstEmptyRow = -1 ;reset, since a row was not empty
        Break
      EndIf
      If RowIsEmpty
        LastEmptyRow = nr
        If FirstEmptyRow = -1
          FirstEmptyRow = nr
        EndIf
      EndIf
    Next nc
  Next nr
  
  If RedimArrayInHere And FirstEmptyRow < rUB And FirstEmptyRow > 0
    ReDim ar2d(cUB,FirstEmptyRow-1)
  EndIf
  
  ProcedureReturn FirstEmptyRow
EndProcedure

Procedure.i RemDuplicatesFromStrArray(Array Arr.s(1), removeBlankLines.i=#False)
  Protected.i i, uB, z, count
  Protected Dim tempArr.s(0)
    
  SortArray(Arr(),#PB_Sort_Ascending)
  
  uB = ArraySize(Arr())
  Dim tempArr(uB)
  
  count=0
  For i = 0 To uB
    z = ArrayPositionStr(tempArr(), arr(i))     
    If z < 0 And (removeBlankLines = #False Or (removeBlankLines =#True And arr(i) <> ""))
      tempArr(count) = Arr(i)
      count+1
    EndIf
  Next i
  
  CopyArrayStr(tempArr(),Arr())
  Dim tempArr(0)
  If count > 0 : count - 1 : EndIf
  If ArraySize(Arr()) > count
    ReDim Arr(count)
  EndIf
  ProcedureReturn count
EndProcedure

Procedure.i ArrayPositionInt(Array arr.i(1), value.i)
;   '=============================================================
;   'Searches array for value
;   ' Returns Location of value if value is in array. -1 if not.
;   '=============================================================  
  ;OnErrorGoto(?errH_LngArrayPosition)
  
  Protected i.i
  
  For i = 0 To ArraySize(arr()) Step 1
    If arr(i) = value
      ProcedureReturn i
    EndIf
  Next i
    
  ;'if this is accessed, then end of array was reached without finding the value
  ProcedureReturn -1
EndProcedure 

Procedure.i EnterDataInArrayInt(Array arr.i(1),value.i, count.i)
  Protected i.i
  
  i = ArrayPositionInt(arr(), value) 
  If i <0
    If count > ArraySize(arr())
      ReDim arr(count)
    EndIf  
    arr(count) = value     
    count+1
  EndIf
  ProcedureReturn count
EndProcedure  

Procedure.i RemDuplicatesFromIntArray(Array Arr.i(1))
  Protected i.i, uB.i, z.i, count.i
  Protected Dim tempArr.i(0)
    
  SortArray(Arr(),#PB_Sort_Ascending)
  
  uB = ArraySize(Arr())
  Dim tempArr(uB)
  
  count=0
  For i = 0 To uB
    z = ArrayPositionInt(tempArr(), arr(i)) 
    If z < 0 
      tempArr(count) = Arr(i)
      count+1
    EndIf
  Next i
  
  CopyArrayI(tempArr(),Arr())
  Dim tempArr(0)
  If count > 0 : count - 1 : EndIf
  If ArraySize(Arr()) > count
    ReDim Arr(count)
  EndIf
  ProcedureReturn count
EndProcedure

;-
;- Array data retrieval
Procedure.i GetLinesFromArray(Array oArr.s(1), Array sFile.s(1), String2Find.s, *LineNum.Integer = 0, StopString.s = "", ReturnFirstLine.i = #False, AllCaps.i = #False, RemoveComments.i = #False) 
  Protected.i iUB, iLine, LineCount, r
  Protected sline.s
  Protected n.i= 1000 ;array redim increment
  
  iUB = ArraySize(sFile())
  
  Dim tempArr.s(0) 
  tempArr(0)=""
  String2Find = UCase(String2Find)
  StopString = UCase(StopString)
  r=1
    
  If *LineNum 
    LineCount = *LineNum\i
  Else
    LineCount = 0
  EndIf
  
  For iLine = LineCount To iUB Step 1
    sline=sfile(iline)
    sFile(iLine) = Trim(sFile(iLine))
    If FindString(UCase(sFile(iLine)), UCase(String2Find), 1) > 0 
      If ReturnFirstLine = #False 
        iline+1
      EndIf     
      Break        
    EndIf
  Next iLine
  
  If (StopString = "") Or (iLine >= iUB + 1) 
    If iLine = iUB + 1 
      ;search parameters not found
      tempArr(0) = #sNULL
      oArr(0) = #sNULL  
      r=0
    Else
      If RemoveComments 
        tempArr(0) = TrimComments(sFile(iLine))
      Else
        tempArr(0) = Trim(sFile(iLine))
      EndIf
      If AllCaps
        tempArr(0) = UCase(tempArr(0))
      EndIf
      r=1
    EndIf

    ProcedureReturn r
  EndIf
  
;   If StopStringSearchOnNextLine
;     iLine + 1
;   EndIf
  LineCount=0
  While FindString(UCase(sFile(iLine)), StopString,1) = 0
    If sFile(iLine) <> ""
      If linecount % n = 0 
        ReDim tempArr(lineCount + n)
      EndIf
      tempArr(lineCount) = Trim(sFile(iLine))
      If AllCaps 
        tempArr(linecount) = UCase(tempArr(linecount))
      EndIf
      linecount + 1
    EndIf
    iLine + 1
    If iLine >= iUB  
      Break
    EndIf
    If RemoveComments 
      sFile(iLine) = TrimComments(sFile(iLine))
    EndIf
  Wend
    
  ;Debug tempArr(linecount)
  If LineCount > 0 : LineCount -1 : EndIf
  ReDim tempArr(LineCount) ;trim array
  
  CopyArrayStr(tempArr(), oArr()) ;copies temp array to output array, 
                                  ; need to do this in case output array and input array are passed in as the same memory location
  
  If *LineNum
    *LineNum\i = iLine
  EndIf
  ProcedureReturn r
EndProcedure

Procedure.i GetCondLinesFromArray(Array oArr.s(1), Array sFile.s(1), String2Find.s, *LineNum.Integer = 0, StopString.s = "", String2Find2.s = "", RemoveComments.i = #True,ReturnFirstLine.i = #False, StopStringSearchOnNextLine.i = #False,  AllCaps.i = #True) 
  Protected.i iUB, iLine, LineCount, r
  Protected sline.s
  Protected n.i= 1000 ;array redim increment
  
  iUB = ArraySize(sFile())
  
  Dim tempArr.s(0) 
  tempArr(0)=""
  String2Find = UCase(String2Find)
  String2Find2 = UCase(String2Find2)
  StopString = UCase(StopString)
  r=1
  
  
  If *LineNum 
    LineCount = *LineNum\i
  Else
    LineCount = 0
  EndIf
  
  For iLine = LineCount To iUB Step 1
    sline=sfile(iline)
    sFile(iLine) = Trim(sFile(iLine))
    If FindString(UCase(sFile(iLine)), UCase(String2Find), 1) > 0 
      If String2Find2 <> "" 
        If FindString(UCase(sFile(iLine)), UCase(String2Find2), 1) > 0 
          If ReturnFirstLine = #False 
            iline+1
          EndIf
          Break
        EndIf
      Else   
        If ReturnFirstLine = #False 
          iline+1
        EndIf     
        Break        
      EndIf
    EndIf
  Next iLine
  
  If (StopString = "") Or (iLine >= iUB + 1) 
    If iLine = iUB + 1 
      tempArr(0) = #sNULL
      r=0
    Else
      If RemoveComments 
        tempArr(0) = TrimComments(sFile(iLine))
      Else
        tempArr(0) = Trim(sFile(iLine))
      EndIf
      If AllCaps
        tempArr(0) = UCase(tempArr(0))
      EndIf
      r=1
    EndIf

    ProcedureReturn r
  EndIf
  
  If StopStringSearchOnNextLine
    iLine + 1
  EndIf
  LineCount=0
  While FindString(UCase(sFile(iLine)), StopString,1) = 0
    If sFile(iLine) <> ""
      If linecount % n = 0 
        ReDim tempArr(lineCount + n)
      EndIf
      tempArr(lineCount) = Trim(sFile(iLine))
      If AllCaps 
        tempArr(linecount) = UCase(tempArr(linecount))
      EndIf
      linecount + 1
    EndIf
    iLine + 1
    If iLine >= iUB  
      Break
    EndIf
    If RemoveComments 
      sFile(iLine) = TrimComments(sFile(iLine))
    EndIf
  Wend
    
  Debug tempArr(linecount)
  If LineCount > 0 : LineCount -1 : EndIf
  ReDim tempArr(LineCount) ;trim array
  
  CopyArrayStr(tempArr(), oArr()) ;copies temp array to output array, 
                                  ; need to do this in case output array and input array are passed in as the same memory location
  
  If *LineNum
    *LineNum\i = iLine
  EndIf
  ProcedureReturn r
EndProcedure

;-
;- Array swapping
Procedure SwapArrayItems_Str(Array arr.s(1), pos1.i, pos2.i)
  ;swaps array(position1) and array(position2) values
  Protected temp.s
  
  temp = arr(pos1)
  arr(pos1)= arr(pos2)
  arr(pos2) = temp  
EndProcedure

Procedure SwapArrayItems_Int(Array arr.i(1), pos1.i, pos2.i)
  ;swaps array(position1) and array(position2) values
  Protected temp.i
  
  temp = arr(pos1)
  arr(pos1)= arr(pos2)
  arr(pos2) = temp  
EndProcedure

;-
;-Write Array to file
Procedure.i strArray1DToFile(Array sArr.s(1),fname.s)
  Protected.i nr,nf,rUB
  Protected del.s= ","
  
  ;header is on row 0 of 2d array
  
  If Not CheckFilename(GetFilePart(fname))
    AssignErrMsg("invalid filename: "+ fname)
    ProcedureReturn 0
  EndIf
  
  nf = CreateFile(#PB_Any,fname)
  If Not nf 
    AssignErrMsg("could not create file: " + fname)
    ProcedureReturn 0
  EndIf
 
  rUB=ArraySize(sArr())
  For nr=0 To rUB  Step 1
    WriteStringN(nf,sArr(nr))
  Next nr  
  
  CloseFile(nf)
  
  ProcedureReturn 1
EndProcedure

Procedure.i strArray2DToFile(Array sArr.s(2),fname.s)
  Protected.i nr,nc,nf,rUB,cUB
  Protected del.s= ","
  
  ;header is on row 0 of 2d array
  
  If Not CheckFilename(GetFilePart(fname))
    AssignErrMsg("invalid filename: "+ fname)
    ProcedureReturn 0
  EndIf
  
  nf = CreateFile(#PB_Any,fname)
  If Not nf 
    AssignErrMsg("could not create file: " + fname)
    ProcedureReturn 0
  EndIf
 
  cUB = ArraySize(sArr(),1)
  rUB=ArraySize(sArr(),2)
  For nr=0 To rUB  Step 1
    For nc=0 To cUB Step 1
      WriteString(nf,sArr(nc,nr))
      If Not nc = cUB
        WriteString(nf,del)
      EndIf
    Next nc
    WriteStringN(nf,"") ;new line
  Next nr  
  
  CloseFile(nf)
EndProcedure


;-
;-{ Steve's 2D Array Functions:
Procedure Ar2DCopy(Array x1.d(2), Array x2.d(2))
  Protected.i nc,nr
  nc = ArraySize(x1(),1)
  nr = ArraySize(x1(),2)
  Dim x2.d(nc,nr)
  CopyMemory(x1(),x2(),SizeOf(DOUBLE) * (nc+1) * (nr+1))  ; Fast method 
EndProcedure

Procedure Ar2DCopyI(Array x1.i(2), Array x2.i(2))
  Protected.i nc,nr ;r,c
  nc = ArraySize(x1(),1)
  nr = ArraySize(x1(),2)
  Dim x2.i(nc,nr)
  CopyMemory(x1(),x2(),SizeOf(INTEGER)*(nc+1)*(nr+1))  ; Fast method 
EndProcedure

Procedure Ar2DCopyS(Array x1.s(2), Array x2.s(2))
  ; Use brute force for string arrays
  Protected.i nc,nr,c,r
  nc = ArraySize(x1(),1)
  nr = ArraySize(x1(),2)
  Dim x2.s(nc,nr)
  For c = 0 To nc
    For r = 0 To nr
      x2(c,r) = x1(c,r)
    Next r
  Next c
EndProcedure

Procedure.i ar2Dto1D(Array x.d(2), Array xm.d(1), nPts.i, Index.i, fixed_elem.i=1)
  ; Convert 2D Array To 1D
  If nPts > 1
    Protected.i i
    Dim xm.d(nPts - 1)
    If fixed_elem = 1
      For i = 0 To nPts - 1
        xm(i) = x(index, i)
      Next i
    Else
      For i = 0 To nPts - 1
        xm(i) = x(i, index)
      Next i
    EndIf
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

Procedure.i ar2Dto1D_S(Array xIN.s(2), Array xOUT.s(1), nPts.i, Index.i, start.i=0, fixed_elem.i=1)
  ; Convert 2D string Array To 1D
  If nPts > 1
    Protected.i i
    Dim xOUT.s(nPts - 1)
    If fixed_elem = 1
      For i = start To (nPts - 1+start)
        xOUT(i-start) = xIN(index, i)
      Next i
    Else
      For i = start To (nPts - 1+start)
        xOUT(i-start) = xIN(i, index)
      Next i
    EndIf
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

Procedure arSortKv3MergePD(Array v.d(1), Array L.i(1), Lbd.i=0)
  ; Numeric Pointer sort in ascending order
  ; Syntax:  arSortKv3MergePD(values(), pointers())
  ; The list-merge algorithm can be found in Knuth, Vol. 3.
  ; Notes:
  ; Use Lbd to adjust lower bounds of operation
  ; Both values and pointers should be one-dimensional arrays.  Both
  ; should have the same lower & upper bounds.  (To be exact, the
  ; values array may have extra elements -- it will be indexed by
  ; the bounds of pointers.)  On return, the values array will be
  ; unchanged, and the pointers array will be filled in with the
  ; indexes of elements of the values array, in order.  That
  ; is, the first element in pointers will be the index of the
  ; smallest element in values, and so forth.
  ; Duplicates are stored with 1st as smallest (the sort is "stable").

  Protected.d vi, vj, vk   ; <--- Change these for type used.
       ; .s for String, .i for Integer, etc.
       ; Remember to also change the Values array type v.d(1)
  Protected.i Ubd
  Protected.i z, zz, L1, L2
  Protected.i krun, kother
  Protected.i i, j, k, Q
  ; Array size check (zero or one elements)
  Ubd = ArraySize(L())
  If Ubd <= Lbd
    If Ubd < Lbd
     ProcedureReturn
    EndIf
    L(Lbd) = Lbd
    ProcedureReturn
  EndIf
  z = Lbd - 1
  zz = 2 * z
  L2 = Lbd
  L1 = L2 + 1
  If v(L1) < v(L2)
    L1 = L2
    L2 = L1 + 1
  EndIf
  krun = L1
  kother = L2
  vj = v(L1)
  vk = v(L1)
  For i = Lbd + 2 To Ubd
    vi = v(i)         
    If vi < vj       
      If vi >= v(L2)
        L(i) = L1
        L1 = i
      Else               
        L(i) = L(L2)
        L(L2) = L1
        L1 = L2
        If kother = L2
          kother = i
        EndIf
        L2 = i
      EndIf
      vj = v(L1)
    Else
      If vi >= vk       
        L(krun) = i     
      Else               
        L(kother) = zz - i
        kother = krun
      EndIf
      krun = i
      vk = vi
    EndIf
  Next i
  L(krun) = z
  L(kother) = z
  L(L1) = zz - L(L1)
  Repeat                 
    i = L(L1)
    j = L(L2)
    If j = z
      Break
    EndIf
    krun = L1
    kother = L2
    Repeat
      i = zz - i
      j = zz - j
      vi = v(i)
      vj = v(j)
      k = krun
      Repeat
        If vi <= vj
          L(k) = i
          k = i
          i = L(i)
          If i <= z
            L(k) = j
            Repeat
              k = j
              j = L(j)
            Until j <= z ; While j > z
            Break
          EndIf
          vi = v(i)
        Else
          L(k) = j
          k = j
          j = L(j)
          If j <= z
            L(k) = i
            Repeat
              k = i
              i = L(i)
            Until i <= z ; While i > z
            Break
          EndIf
          vj = v(j)
        EndIf
      ForEver
      L(krun) = zz - L(krun)   
      krun = kother   
      kother = k     
    Until j = z
    L(krun) = i
    L(kother) = z
  ForEver
  L(L2) = L1
  L(L1) = zz - L(L1)
  i = L2
  For j = zz - Lbd To zz - Ubd Step -1
    k = L(L2)
    L(L2) = j
    L2 = k
  Next j
  For i = Lbd To Ubd
    j = L(i)
    If j < z
      j = zz - j
      Repeat
        Q = zz - L(j)
        L(j) = k
        k = j
        j = Q
      Until j <= z ; While j > z
    EndIf
  Next i
EndProcedure

Procedure arSortKv3MergeP_S(Array v.s(1), Array L.i(1), Lbd.i=0)
  ; Numeric Pointer sort in ascending order
  ; Syntax:  arSortKv3MergePD(values(), pointers())
  ; The list-merge algorithm can be found in Knuth, Vol. 3.
  ; Notes:
  ; Use Lbd to adjust lower bounds of operation
  ; Both values and pointers should be one-dimensional arrays.  Both
  ; should have the same lower & upper bounds.  (To be exact, the
  ; values array may have extra elements -- it will be indexed by
  ; the bounds of pointers.)  On return, the values array will be
  ; unchanged, and the pointers array will be filled in with the
  ; indexes of elements of the values array, in order.  That
  ; is, the first element in pointers will be the index of the
  ; smallest element in values, and so forth.
  ; Duplicates are stored with 1st as smallest (the sort is "stable").

  Protected.s vi, vj, vk   ; <--- Change these for type used.
       ; .s for String, .i for Integer, etc.
       ; Remember to also change the Values array type v.d(1)
  Protected.i Ubd
  Protected.i z, zz, L1, L2
  Protected.i krun, kother
  Protected.i i, j, k, Q
  ; Array size check (zero or one elements)
  Ubd = ArraySize(L())
  If Ubd <= Lbd
    If Ubd < Lbd
     ProcedureReturn
    EndIf
    L(Lbd) = Lbd
    ProcedureReturn
  EndIf
  z = Lbd - 1
  zz = 2 * z
  L2 = Lbd
  L1 = L2 + 1
  If v(L1) < v(L2)
    L1 = L2
    L2 = L1 + 1
  EndIf
  krun = L1
  kother = L2
  vj = v(L1)
  vk = v(L1)
  For i = Lbd + 2 To Ubd
    vi = v(i)         
    If vi < vj       
      If vi >= v(L2)
        L(i) = L1
        L1 = i
      Else               
        L(i) = L(L2)
        L(L2) = L1
        L1 = L2
        If kother = L2
          kother = i
        EndIf
        L2 = i
      EndIf
      vj = v(L1)
    Else
      If vi >= vk       
        L(krun) = i     
      Else               
        L(kother) = zz - i
        kother = krun
      EndIf
      krun = i
      vk = vi
    EndIf
  Next i
  L(krun) = z
  L(kother) = z
  L(L1) = zz - L(L1)
  Repeat                 
    i = L(L1)
    j = L(L2)
    If j = z
      Break
    EndIf
    krun = L1
    kother = L2
    Repeat
      i = zz - i
      j = zz - j
      vi = v(i)
      vj = v(j)
      k = krun
      Repeat
        If vi <= vj
          L(k) = i
          k = i
          i = L(i)
          If i <= z
            L(k) = j
            Repeat
              k = j
              j = L(j)
            Until j <= z ; While j > z
            Break
          EndIf
          vi = v(i)
        Else
          L(k) = j
          k = j
          j = L(j)
          If j <= z
            L(k) = i
            Repeat
              k = i
              i = L(i)
            Until i <= z ; While i > z
            Break
          EndIf
          vj = v(j)
        EndIf
      ForEver
      L(krun) = zz - L(krun)   
      krun = kother   
      kother = k     
    Until j = z
    L(krun) = i
    L(kother) = z
  ForEver
  L(L2) = L1
  L(L1) = zz - L(L1)
  i = L2
  For j = zz - Lbd To zz - Ubd Step -1
    k = L(L2)
    L(L2) = j
    L2 = k
  Next j
  For i = Lbd To Ubd
    j = L(i)
    If j < z
      j = zz - j
      Repeat
        Q = zz - L(j)
        L(j) = k
        k = j
        j = Q
      Until j <= z ; While j > z
    EndIf
  Next i
EndProcedure

Procedure.i MergeSort2DArray(Array sortedArr2D.d(2),Array origArr2D.d(2),numCols.i,nRow.i,ColToSort.i)
  ;origArr2D() is preserved, sortedArr2D() is returned as a copy of the information from the original array (but sorted)
  Protected Dim arr1D.d(nRow-1)
  Protected Dim arrPtrs.i(nRow-1)
  Protected.i i,j,ri
 
  ri = ar2Dto1D(origArr2D(), arr1D(), nRow, ColToSort)  ; Grab the specified column of data
  arSortKv3MergePD(arr1D(), arrPtrs())          ; aP() contains the Final sorted Pointer values
                                                ; Xsrch() is unchanged
  For i = 0 To numCols-1
    For j = 0 To nRow-1
      sortedArr2D(i, j) = origArr2D(i, arrPtrs(j))
    Next j
  Next i   
  ProcedureReturn ri                                   
EndProcedure

;}











; IDE Options = PureBasic 4.50 (Windows - x86)
; CursorPosition = 382
; FirstLine = 348
; Folding = ---H+
; EnableAsm
; EnableXP
; EnableAdmin
; EnableOnError
; CurrentDirectory = C:\Users\jrobbins\Documents\aPBP\PureBasic\
; EnableBuildCount = 0
; EnableExeConstant