; AUTHOR:  Jon Robbins / jamba
; ================================================
; www.jrobb.org
; ================================================
; This code is free to share and modify under the GPL v3 or > at your option.
; (if possible)
; =========================
; This code was written with PureBasic 4.50
;   www.purebasic.com
; =========================
; 
;
;============================================================================
;WordPress Conversion routines and such.  not clean, very quickly hacked
;=============================================================================
EnableExplicit
#WPXML_To_HTML = 44
#WP_TagChar = "#"
#WP_TagDel = " "
#sNULL = "-999"

Define cli_arg.s
Global errbuf.s

Procedure AssignErrMsg(msg.s)
  If MSG
    errbuf + " : " + msg
  EndIf
  If ErrorCode()
    errbuf + " " + ErrorMessage()
  EndIf
EndProcedure

Macro FileExists(fname)
  FileSize(fname)
EndMacro
XIncludeFile "lib_string-1.2.pbi"

Procedure ReadFile2Array(Array sarr.s(1), fname.s, RemoveComments.i = #False, Commenter.s = "!")
  ;trond's take on the readfile2array proc.
  ;reads in the entire file at once, before using pointers to look at the string in memory, split it at the correct places and put the lines into the array:
  ;Forum link: http://www.purebasic.fr/english/viewtopic.php?f=13&t=40675&start=0  
  Protected.i Length, n, I, JumpCR
  Protected File.s
  Protected *Memory.Character, *Linestart
  
  If ReadFile(0, fname)
    Length = Lof(0)
    File = Space(Length)
    ReadData(0, @File, Length) ; Read in the entire file at once
    CloseFile(0)
    n=CountString(File, #LF$)-1
    Dim sarr(n)
    *Memory = @File
    *Linestart = *Memory
    While *Memory\c
      ;Read until linefeed
      If *Memory\c <> #LF
        *Memory + 1          
      Else
        ; Handle the optional CR part of CRLF
        *Memory - 1
        If *Memory\c = #CR
          JumpCR = 1
        Else
          *Memory + 1
          JumpCR = 0
        EndIf
        ; Copy string into array
        If RemoveComments
          ;Trim out the whitespace and any comments, leading or trailing
          sarr(I) = TrimComments(PeekS(*Linestart, *Memory-*Linestart),Commenter)
        Else
          ;just trim the whitespace
          sarr(I) = Trim(PeekS(*Linestart, *Memory-*Linestart))
        EndIf
        *Linestart = *Memory+1 + JumpCR
        *Memory + 2 + JumpCR
        If (sarr(I) <> "") : I + 1 : EndIf ;overwrite blank strings
      EndIf  
    Wend
    If n > (I-1)
      ;This means that blank lines were present, and overwritten.  
      ;ReDimming will shorten the array, and eliminate the white space.
      ReDim sarr(I-1)
    EndIf
  EndIf
EndProcedure

;-requires:

XIncludeFile "lib_array-1.3.pbi"
;-

Procedure CreateDir(Directory$)
  ;CreateDirectory Paths if they don't exist
   Protected Dim a.s(0), size.i, b.s, i.i
   Protected del.s
   Protected delReplace.s 
   CompilerIf #PB_Compiler_OS = #PB_OS_Windows
    del = "\"
    delReplace= "/"
   CompilerElse
    del = "/"
    delReplace = "\"
   CompilerEndIf
   
   ReplaceString(Directory$, delReplace, del)
   If Right(Directory$,1) = del
      Directory$ = Left(Directory$,Len(Directory$)-1)
   EndIf
   
   size = explodeArray(a(), Directory$,del)
   
   For i = 0 To size
     b + a(i) + del
     ExamineDirectory(0, b, "*.*")
    
     If Not IsDirectory(0)
         CreateDirectory(b)
      EndIf
   Next
EndProcedure



;-

Structure struct_WPpost
  ;Each post separated inside of <item></item> tags
  Title.s         ;title /title
  Link.s          ;link /link
  pubDate.s       ;pubDate /
  creator.s       ;<dc:creator><![CDATA[jamba]]></dc:creator>
  category.s      ;<category><![CDATA[life]]></category>
  tagString.s     ;#tag1 #tag2 ...
  guid.s          ;<guid isPermaLink="false"> </guid>
  content.s       ;<content:encoded><![CDATA[  ... ]]></content:encoded>
  wp_postID.s     ;wp:post_id /
  wp_postDate.s   ;wp:post_date /
  wp_postName.s   ;wp:post_name /
EndStructure

Procedure.s WP_getPostTags(Array arItem.s(1))
  ;use this first one to grab tag:
  ; <category domain="tag"><![CDATA[cloud]]></category>
  ; <category domain="tag" nicename="cloud"><![CDATA[cloud]]></category>
  Protected i.i 
  Protected s.s
  Protected TagString.s 
  Protected xmlTag.s = "<category domain=" + #DQUOTE$ + "tag" + #DQUOTE$ + ">"
  For i = 0 To ArraySize(arItem())  
    If FindString(arItem(i),xmlTag,1)
      s= GetStrBetween(arItem(i),"<![CDATA[", "]]>")
      If s
        TagString = TagString + #WP_TagDel + #WP_TagChar + ReplaceString(s," ","-")
      EndIf
    EndIf
  Next i
  ProcedureReturn TagString
EndProcedure

Procedure.s WP_getPostContent(Array arItem.s(1))
  Protected i.i 
  Protected s.s
  Protected postContent.s
  Protected xmlTag.s = "<content:encoded>"
  Protected xmlTagClose.s = "</content:encoded>"  
  For i = 0 To ArraySize(arItem())  
    If FindString(arItem(i),xmlTag,1)
      Repeat
        s= arItem(i)
        If s
          If FindString(s,xmlTag + "<![CDATA[",1)
            s = ReplaceString(s,xmlTag + "<![CDATA[","")
          EndIf
          If FindString(s,"]]></content:encoded>",1)
            s = ReplaceString(s,"]]></content:encoded>","")          
          EndIf
          If FindString(s,Chr(-62),1)
            s= ReplaceString(s,Chr(-62)," ") ;delete this odd character
          EndIf
          postContent = postContent + s + #LF$
          If Right(s,1) <> ">" ;check for html tag
            postContent + "<br> "
          EndIf
          s=""
        EndIf
        
        If  FindString(arItem(i),xmlTagClose,1)
          Break
        EndIf
        i+1
      Until i > ArraySize(arItem())
      Break
    EndIf
  Next i
  ProcedureReturn postContent
EndProcedure

Procedure.s WP_getXMLValue(tag.s,Array arItem.s(1))
  Protected i.i 
  Protected s.s 
  For i = 0 To ArraySize(arItem())    
    Select tag      
    Case "creator"  ;<dc:creator><![CDATA[jamba]]></dc:creator>
      If FindString(arItem(i),"<dc:" + tag + ">",1)
        s= GetStrBetween(arItem(i),"<dc:" + tag + "><![CDATA[","]]></dc:" + tag + ">")
        Break
      EndIf
    Case "category" ;<category><![CDATA[life]]></category>
      If FindString(arItem(i),"<" + tag + ">",1)
        s= GetStrBetween(arItem(i),"<" + tag + "><![CDATA[","]]></" + tag + ">")
        Break
      EndIf
    Case "guid" ;<guid isPermaLink="false"> </guid>
      If FindString(arItem(i),"<" + tag,1)
        s= GetStrBetween(arItem(i),">","</" + tag + ">")
        Break
      EndIf
    Default ;title, link, pubDate, wp:post_id, wp:post_date, wp:post_name
      ;simple, just need to get str between
      If FindString(arItem(i),"<"+tag + ">",1)
        s= GetStrBetween(arItem(i),"<" + tag + ">","</" + tag + ">")
        Break
      EndIf
    EndSelect
  Next i
  ProcedureReturn s
EndProcedure

Procedure.i WP_ToHLscriptHTMLPost(of.s,*Post.struct_WPpost)
  Protected fnum.i
  fnum = CreateFile(#PB_Any,of)
  
  With *Post
    WriteStringN(fnum,\wp_postDate) 
    WriteStringN(fnum,"")
    WriteStringN(fnum,"\" + \Title)
    WriteStringN(fnum,"")
    WriteStringN(fnum, \content)
    WriteStringN(fnum,"")
    WriteStringN(fnum,"Posted by: " + \creator)
    WriteStringN(fnum,"")
    WriteStringN(fnum,"Category: ##" + \category + " <br>")
    If \tagString
      WriteStringN(fnum,"Tags: " + \tagString + " <br>")  
    EndIf
    WriteStringN(fnum,"")
    WriteStringN(fnum,"Published Date: " + \pubDate + " <br>")    
    WriteString(fnum,"<a href=" + #DQUOTE$ + \Link + #DQUOTE$ + ">Original URL</a> | ")
    WriteString(fnum,"<a href=" + #DQUOTE$ + \guid  + #DQUOTE$ + ">Original guid</a> | ")
    WriteStringN(fnum,"PostID= " + \wp_postID)
    
  EndWith
  CloseFile(fnum)
  ProcedureReturn 1
EndProcedure

Procedure.i WP_ConvertXMLToHTMLfiles(fName.s, background.i = 0)
  Dim arFile.s(0) ; file in array
  Dim arSection.s(0) ;sub section (<item> to </item>)
  Protected postCount.i 
  Protected lineCount.i ;line counter
  Protected i.i ;line counter (in section)
  Protected UB.i
  Protected wpPost.struct_WPpost
  Protected ofName.s
  Protected ofDir.s    
  If Not FileExists(fName) : ProcedureReturn 0 : EndIf
  
  ReadFile2Array(arFile(),fName)
  ofDir = GetPathPart(fName) + Left(GetFilePart(fName),Len(GetFilePart(fName))-4) + "_HTML\"
  CreateDir(ofDir)
  UB= ArraySize(arFile())
  lineCount=0
  postCount = 1
  Repeat
    If GetLinesFromArray(arSection(),arFile(),"<item>",@lineCount,"</item>",0,0,0)
      ClearStructure(wpPost,struct_WPpost) ;clear it out
      With wpPost
        \Title = WP_getXMLValue("title",arSection())
        \Link = WP_getXMLValue("link",arSection())
        \pubDate = WP_getXMLValue("pubDate",arSection())
        \creator = WP_getXMLValue("creator",arSection())
        \category = WP_getXMLValue("category",arSection())        
        \guid = WP_getXMLValue("guid",arSection())        
        \wp_postID = WP_getXMLValue("wp:post_id",arSection())
        \wp_postDate = WP_getXMLValue("wp:post_date",arSection())
        \wp_postName = WP_getXMLValue("wp:post_name",arSection())
        
        \tagString = WP_getPostTags(arSection())
        \content = WP_getPostContent(arSection())
      EndWith
      ofName = ofDir + Str(postCount)
      WP_ToHLscriptHTMLPost(ofName,wpPost)      
      postCount + 1
    Else
      Break
      ;no more found
    EndIf
  Until lineCount >= UB 
  ProcedureReturn 1
EndProcedure

Procedure Close(closeConsole.i=1)
  Delay(1000)
  CloseConsole()
  End
EndProcedure

  
;- MAIN  
If Not OpenConsole()
  End
EndIf

cli_arg = Trim(ProgramParameter())
If Not cli_arg
  PrintN("No file as input for conversion...exiting")
  Close()
EndIf
If FileSize(cli_arg) <= 0
  PrintN("not a valid file: " + cli_arg)
  Close()    
EndIf

If WP_ConvertXMLToHTMLfiles(cli_arg, #True) 
  PrintN("Done!")
Else
  PrintN("there was an error... ")
EndIf

Close()

; IDE Options = PureBasic 4.50 (Windows - x86)
; CursorPosition = 306
; FirstLine = 175
; Folding = 7z
; EnableAsm
; EnableXP
; EnableOnError
; Executable = C:\Users\jrobbins\Documents\Code_Projects\tests\wp_xml_converter\wp_xml_converter.exe
; CurrentDirectory = C:\Users\jrobbins\Documents\aPBP\PureBasic\
; EnableBuildCount = 1
; EnableExeConstant