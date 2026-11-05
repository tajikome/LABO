Attribute VB_Name = "PhotoLedger"
Option Explicit

' 写真台帳(A4縦 1ページ10枚: 左列 No.1-5 / 右列 No.6-10)に写真を自動で配置するマクロ
' ・フォルダ名がそのままシート名になります(シートは自動で追加)
' ・写真の枚数に合わせてページを自動で増減します(30枚超えは自動でページ追加)
' ・最後の写真より後ろの枠は作りません(14枚なら No.14 で終了)
' PhotoFillFolder : 選んだフォルダの中のサブフォルダ(例 1F, 2F, 10F)ごとにシートを作り、写真を配置
'                   (サブフォルダが無く、写真が直接入っている場合は、そのフォルダ名のシート1枚)
' PhotoFillSheet  : 選んだ写真を、今開いているシートに配置
' PhotoClear      : 今開いているシートの写真をすべて削除

' 行の高さは3の倍数(ポイント)にしてあります。画面の拡大率(125%など)でも
' 行が整数ピクセルになり、写真が枠からずれるのを防ぐためです。
Private Const ROW_LABEL As Single = 15
Private Const ROW_PHOTO As Single = 132
Private Const COL_WIDTH_PT As Single = 261

Private Function SlotCell(ws As Worksheet, ByVal n As Long) As Range
    Dim p As Long, k As Long, ci As Long, r As Long
    p = (n - 1) \ 10
    k = (n - 1) Mod 10
    ci = k \ 5
    r = k Mod 5
    Set SlotCell = ws.Cells(p * 10 + r * 2 + 2, 1 + ci)
End Function

Private Sub BuildSlot(ws As Worksheet, ByVal n As Long, ByVal used As Boolean)
    Dim p As Long, k As Long, ci As Long, r As Long, lr As Long
    Dim lc As Range, pc As Range
    p = (n - 1) \ 10
    k = (n - 1) Mod 10
    ci = k \ 5
    r = k Mod 5
    lr = p * 10 + r * 2 + 1
    ws.Rows(lr).RowHeight = ROW_LABEL
    ws.Rows(lr + 1).RowHeight = ROW_PHOTO
    If Not used Then Exit Sub
    Set lc = ws.Cells(lr, 1 + ci)
    Set pc = ws.Cells(lr + 1, 1 + ci)
    lc.Value = "No." & n
    lc.Font.Name = "Meiryo UI"
    lc.Font.Size = 9
    lc.Font.Bold = True
    lc.Interior.Color = RGB(231, 230, 230)
    lc.VerticalAlignment = xlCenter
    lc.BorderAround xlContinuous, xlThin, , RGB(128, 128, 128)
    pc.BorderAround xlContinuous, xlThin, , RGB(128, 128, 128)
End Sub

Private Sub SetupSheet(ws As Worksheet)
    Dim i As Long, k As Long
    For k = 1 To 3
        For i = 1 To 2
            ws.Columns(i).ColumnWidth = ws.Columns(i).ColumnWidth * COL_WIDTH_PT / ws.Columns(i).Width
        Next i
    Next k
    On Error Resume Next
    With ws.PageSetup
        .PaperSize = xlPaperA4
        .Orientation = xlPortrait
        .Zoom = 100
        .LeftMargin = Application.InchesToPoints(0.3)
        .RightMargin = Application.InchesToPoints(0.3)
        .TopMargin = Application.InchesToPoints(0.75)
        .BottomMargin = Application.InchesToPoints(0.4)
        .HeaderMargin = Application.InchesToPoints(0.8)
        .FooterMargin = Application.InchesToPoints(0.15)
        .CenterHorizontally = True
        .CenterVertically = True
        .CenterHeader = "&14&A 写真リスト"
        .CenterFooter = "&P / &N"
    End With
    On Error GoTo 0
End Sub

Private Sub ClearPictures(ws As Worksheet)
    Dim i As Long
    For i = ws.Shapes.Count To 1 Step -1
        If ws.Shapes(i).Type = msoPicture Then ws.Shapes(i).Delete
    Next i
End Sub

Private Function HasPictures(ws As Worksheet) As Boolean
    Dim s As Shape
    For Each s In ws.Shapes
        If s.Type = msoPicture Then HasPictures = True: Exit Function
    Next s
End Function

Private Sub PutPicture(ws As Worksheet, ByVal n As Long, ByVal path As String)
    Dim c As Range, shp As Shape, mg As Single, w As Single, h As Single
    Set c = SlotCell(ws, n)
    mg = 3
    Set shp = ws.Shapes.AddPicture(path, msoFalse, msoTrue, c.Left, c.Top, -1, -1)
    shp.LockAspectRatio = msoTrue
    w = c.Width - mg * 2
    h = c.Height - mg * 2
    If shp.Width / shp.Height > w / h Then
        shp.Width = w
    Else
        shp.Height = h
    End If
    shp.Left = c.Left + (c.Width - shp.Width) / 2
    shp.Top = c.Top + (c.Height - shp.Height) / 2
    shp.Placement = xlMove
End Sub

' 数字の並びを桁そろえして、1F 2F ... 10F や IMG_2 IMG_10 を自然な順に並べる
Private Function NatKey(ByVal s As String) As String
    Dim i As Long, c As String, run As String, res As String
    s = LCase$(s)
    For i = 1 To Len(s)
        c = Mid$(s, i, 1)
        If c >= "0" And c <= "9" Then
            run = run & c
        Else
            If Len(run) > 0 Then
                If Len(run) < 10 Then res = res & String$(10 - Len(run), "0")
                res = res & run
                run = ""
            End If
            res = res & c
        End If
    Next i
    If Len(run) > 0 Then
        If Len(run) < 10 Then res = res & String$(10 - Len(run), "0")
        res = res & run
    End If
    NatKey = res
End Function

Private Sub SortNatural(a() As String, ByVal cnt As Long)
    Dim k() As String, i As Long, j As Long, t As String
    If cnt < 2 Then Exit Sub
    ReDim k(1 To cnt)
    For i = 1 To cnt
        k(i) = NatKey(a(i))
    Next i
    For i = 1 To cnt - 1
        For j = i + 1 To cnt
            If StrComp(k(i), k(j), vbBinaryCompare) > 0 Then
                t = a(i): a(i) = a(j): a(j) = t
                t = k(i): k(i) = k(j): k(j) = t
            End If
        Next j
    Next i
End Sub

Private Function IsImage(ByVal f As String) As Boolean
    Dim e As String
    If InStrRev(f, ".") = 0 Then Exit Function
    e = LCase$(Mid$(f, InStrRev(f, ".") + 1))
    IsImage = (e = "jpg" Or e = "jpeg" Or e = "png" Or e = "bmp" Or e = "gif")
End Function

Private Function ListImages(ByVal folder As String, files() As String) As Long
    Dim f As String, cnt As Long
    ReDim files(1 To 5000)
    f = Dir(folder & "\*.*")
    Do While f <> ""
        If IsImage(f) And cnt < 5000 Then
            cnt = cnt + 1
            files(cnt) = folder & "\" & f
        End If
        f = Dir()
    Loop
    SortNatural files, cnt
    ListImages = cnt
End Function

Private Function ListSubfolders(ByVal base As String, names() As String) As Long
    Dim f As String, cnt As Long
    ReDim names(1 To 500)
    f = Dir(base & "\*", vbDirectory)
    Do While f <> ""
        If f <> "." And f <> ".." Then
            If (GetAttr(base & "\" & f) And vbDirectory) <> 0 And cnt < 500 Then
                cnt = cnt + 1
                names(cnt) = f
            End If
        End If
        f = Dir()
    Loop
    SortNatural names, cnt
    ListSubfolders = cnt
End Function

Private Function SafeSheetName(ByVal s As String) As String
    Dim ch As Variant
    For Each ch In Array("\", "/", "?", "*", "[", "]", ":")
        s = Replace(s, CStr(ch), "_")
    Next ch
    SafeSheetName = Left$(s, 31)
End Function

Private Function GetOrAddSheet(ByVal nm As String) As Worksheet
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(nm)
    On Error GoTo 0
    If ws Is Nothing Then
        Set ws = ThisWorkbook.Worksheets.Add(After:=ThisWorkbook.Worksheets(ThisWorkbook.Worksheets.Count))
        ws.Name = nm
    End If
    Set GetOrAddSheet = ws
End Function

' 枚数に合わせて枠(ページ)を作り直し、写真を配置する。戻り値=ページ数
Private Function FillSheet(ws As Worksheet, files() As String, ByVal cnt As Long) As Long
    Dim need As Long, tot As Long, i As Long, lastRow As Long
    need = (cnt + 9) \ 10
    If need < 1 Then need = 1
    tot = need * 10
    ClearPictures ws
    lastRow = ws.UsedRange.Row + ws.UsedRange.Rows.Count - 1
    If lastRow < tot Then lastRow = tot
    ws.Range("A1:B" & CStr(lastRow)).UnMerge
    ws.Range("A1:B" & CStr(lastRow)).Clear
    SetupSheet ws
    If lastRow > tot Then ws.Rows(CStr(tot + 1) & ":" & CStr(lastRow)).RowHeight = ws.StandardHeight
    For i = 1 To tot
        BuildSlot ws, i, (i <= cnt)
    Next i
    For i = 1 To cnt
        PutPicture ws, i, files(i)
    Next i
    On Error Resume Next
    ws.PageSetup.PrintArea = "$A$1:$B$" & CStr(tot)
    ws.ResetAllPageBreaks
    For i = 1 To need - 1
        ws.HPageBreaks.Add ws.Cells(i * 10 + 1, 1)
    Next i
    ws.Activate
    ActiveWindow.DisplayGridlines = False
    On Error GoTo 0
    FillSheet = need
End Function

Sub PhotoFillFolder()
    Dim base As String, files() As String, subs() As String
    Dim nSub As Long, cnt As Long, pg As Long, i As Long, made As Long
    Dim msg As String, ws As Worksheet, firstWs As Worksheet
    With Application.FileDialog(msoFileDialogFolderPicker)
        .Title = "階ごとのフォルダ(1F, 2F ...)が入っているフォルダを選択"
        If .Show <> -1 Then Exit Sub
        base = .SelectedItems(1)
    End With
    If MsgBox("同じ名前のシートがある場合は、枠と写真を作り直します。よろしいですか?", vbOKCancel + vbQuestion) <> vbOK Then Exit Sub
    Application.ScreenUpdating = False
    nSub = ListSubfolders(base, subs)
    For i = 1 To nSub
        cnt = ListImages(base & "\" & subs(i), files)
        If cnt > 0 Then
            Set ws = GetOrAddSheet(SafeSheetName(subs(i)))
            pg = FillSheet(ws, files, cnt)
            If firstWs Is Nothing Then Set firstWs = ws
            made = made + 1
            msg = msg & ws.Name & ": " & cnt & "枚(" & pg & "ページ)" & vbCrLf
        End If
    Next i
    If made = 0 Then
        cnt = ListImages(base, files)
        If cnt > 0 Then
            Set ws = GetOrAddSheet(SafeSheetName(Mid$(base, InStrRev(base, "\") + 1)))
            pg = FillSheet(ws, files, cnt)
            Set firstWs = ws
            made = 1
            msg = ws.Name & ": " & cnt & "枚(" & pg & "ページ)" & vbCrLf
        End If
    End If
    If made > 0 Then
        Application.DisplayAlerts = False
        On Error Resume Next
        If ThisWorkbook.Worksheets.Count > 1 Then ThisWorkbook.Worksheets("準備").Delete
        On Error GoTo 0
        Application.DisplayAlerts = True
        If Not firstWs Is Nothing Then firstWs.Activate
    Else
        msg = "写真(jpg/png など)が入ったフォルダが見つかりませんでした。"
    End If
    Application.ScreenUpdating = True
    MsgBox msg, vbInformation
End Sub

Sub PhotoFillSheet()
    Dim v As Variant, files() As String, cnt As Long, i As Long, pg As Long
    v = Application.GetOpenFilename("画像ファイル,*.jpg;*.jpeg;*.png;*.bmp;*.gif", , "写真を選択(複数選択可)", , True)
    If Not IsArray(v) Then Exit Sub
    cnt = UBound(v) - LBound(v) + 1
    ReDim files(1 To cnt)
    For i = 1 To cnt
        files(i) = v(LBound(v) + i - 1)
    Next i
    SortNatural files, cnt
    If HasPictures(ActiveSheet) Then
        If MsgBox("このシートの既存の写真と枠を削除して、配置し直します。よろしいですか?", vbOKCancel + vbQuestion) <> vbOK Then Exit Sub
    End If
    Application.ScreenUpdating = False
    pg = FillSheet(ActiveSheet, files, cnt)
    Application.ScreenUpdating = True
    MsgBox cnt & "枚を配置しました。(" & pg & "ページ)", vbInformation
End Sub

Sub PhotoClear()
    If MsgBox("このシートの写真をすべて削除します。よろしいですか?", vbOKCancel + vbQuestion) <> vbOK Then Exit Sub
    ClearPictures ActiveSheet
End Sub
