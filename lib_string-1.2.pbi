; AUTHOR:  Jon Robbins
; ================================================
; RFMD
; 7858 Thorndike Rd
; Greensboro, NC  27409         jrobbins@rfmd.com
; p: 336-678-5364               c: 336-314-7262
; ================================================

;============================================================================
;String functions
;=============================================================================
;2010.01.27
EnableExplicit

Procedure.s TrimComments(s.s, Commenter.s = "!")
  Protected.i pos
  
  s=Trim(s)
  pos = FindString(s, Commenter,1)
    
  If pos = 0 ;no comments    
    ProcedureReturn s
  ElseIf pos = 1 ;whole line is a comment
    ProcedureReturn "" ;return blank line
  EndIf
    
  ProcedureReturn Trim(Left(s, pos - 1))
EndProcedure

Procedure.s TrimExtraChars(txt.s, char2Trim.c = ',')    
  If 1=1 ;use PB functions
    txt =LTrim(txt,Chr(char2Trim))
    txt = RTrim(txt,Chr(char2Trim))
    ProcedureReturn txt
  EndIf
  
  Protected *Start.Character, *Stop.Character
  
  *Start = @txt
  *Stop = *Start + Len(txt) -1  
  If *stop < *Start
    ProcedureReturn ""
  EndIf
  
  While *Start And *Start\c = char2Trim  ;eliminate extra chars from the front
    *Start+1
  Wend
  
  While *Stop And *Stop\c = char2Trim ;eliminate extra chars from the back
    *Stop -1
  Wend
  
  ProcedureReturn PeekS(*Start,*Stop-*Start+1)
EndProcedure

Procedure.s GetStrBetween (WholeString.s, FirstVal.s, SecondVal.s) 
  WholeString = Right(WholeString, Len(WholeString) - (FindString(WholeString, FirstVal,1) + Len(FirstVal) - 1))
  ProcedureReturn Left(WholeString, (FindString(wholeString, SecondVal,1) - 1))
EndProcedure 

Procedure SplitStringByCharWidth(Array sarr.s(1), txt.s, maxWidth.i = 0)
  Protected n.l, count.l
  Protected *Memory.Character, *Linestart
  Protected I
  Protected JumpCR.i
    
    
  If Len(txt) <= maxWidth
    Dim sarr(0)
    sarr(0) = txt
    ProcedureReturn
  EndIf
    
  n=CountString(txt, #LF$)-1 + Round(Len(txt)/maxWidth,#PB_Round_Up)
  Dim sarr(n)
  *Memory = @txt
  *Linestart = *Memory
  count=1 ;increments up to maxwidth, then back to 1
  i = 0
  Repeat    
    If *Memory\c And *Memory\c <> #LF And (count < maxWidth Or count=0)
      *Memory + 1          
      count+1
    Else
      Debug Chr(*Memory\c)
      If (count = maxWidth) And Not *Memory\c = ' '        
        If FindString(Trim(PeekS(*Linestart, *Memory-*Linestart))," ",1)  ;if there actually is a space in there
          While Not *Memory\c = ' ' 
            *Memory -1
            Debug Chr(*Memory\c)
          Wend
        EndIf
        Debug Chr(*Memory\c)
        JumpCR = 0
      Else    
        ; Handle the optional CR part of CRLF
        *Memory - 1
        If *Memory\c = #CR
          JumpCR = 1
        Else
          *Memory + 1
          JumpCR = 0
        EndIf
      EndIf
      
      ; Copy string into array  
      If Not *Memory\c    
        While Not *Memory\c
          *Memory - 1
        Wend    
        Debug Chr(*Memory\c)
        *Memory + 1 ;to not lose last character  
        Debug Chr(*Memory\c)
        sarr(I) = Trim(PeekS(*Linestart, *Memory-*Linestart))                   
        i+1
        Break
      EndIf
      
      sarr(I) = Trim(PeekS(*Linestart, *Memory-*Linestart))                   
      
      *Linestart = *Memory+1 + JumpCR
      *Memory + 2 + JumpCR
      count = 1
      I+1           
    EndIf
  ForEver
  
  If n > (I-1)
    ;ReDimming will shorten the array, and eliminate the white space.
    ReDim sarr(I-1)
  EndIf
EndProcedure

Procedure.i ByRefString(*buffer, newValue.s)
  Protected.i bufLength = MemoryStringLength(*buffer)
  If Len(newValue) > bufLength
    PokeS(*buffer, Left(newValue,bufLength-1) + "~") ;truncated value
    ProcedureReturn 0
  EndIf
  
  PokeS(*buffer, newValue, bufLength)
  
  ProcedureReturn 1
EndProcedure

Procedure.s Num2Alpha(num.i) ;enters a number, returns A-Z equivalent. A=1, Z=26.
  Protected i.i
  Protected s.s
  If num <=0
    ProcedureReturn ""
  EndIf
  s="A"
  s= Chr(Asc(s) + num -1)
  ProcedureReturn s
EndProcedure
; IDE Options = PureBasic 4.50 (Windows - x86)
; CursorPosition = 154
; FirstLine = 52
; Folding = 4-
; EnableAsm
; EnableXP
; EnableAdmin
; EnableOnError
; DisableDebugger
; CurrentDirectory = C:\Users\jrobbins\Documents\aPBP\PureBasic\
; EnableBuildCount = 0
; EnableExeConstant