Attribute VB_Name = "Module1"
'===================================================================
'  写真自動配置
'
'  フォルダを選ぶと、そのフォルダの中の画像ファイルを、
'  「写真」タブのC列に、ファイル名(拡張子を除いた部分)と
'  什器No(A列)が一致する行へ、自動で配置します。
'
'  例: フォルダの中に「X1.jpg」があれば、什器Noが「X1」の行の
'      C列に配置されます。
'
'  使い方:
'    1. Alt+F11 でVBAエディタを開く
'    2. 「挿入」→「標準モジュール」(または、このファイルをインポート)
'    3. このコードを貼り付ける
'    4. Alt+F8 で「写真を自動配置」を選び、実行する
'    5. 写真が入っているフォルダを選ぶ
'
'  注意:
'    ・対応する拡張子は jpg, jpeg, png, bmp, gif です
'    ・シート名「写真」、什器NoがA列であることを前提にしています
'      (シート名が違う場合は、下の SHEET_NAME を書き換えてください)
'    ・すでにC列に写真が入っている行は、いったん削除してから入れ直します
'    ・マクロを使うには、ファイルを「Excel マクロ有効ブック(.xlsm)」
'      として保存し直す必要があります
'===================================================================

Const SHEET_NAME As String = "写真"
Const KEY_COL As Long = 1   ' 什器No の列(A列)
Const PIC_COL As Long = 3   ' 写真を置く列(C列)

Sub 写真を自動配置()
    Dim ws As Worksheet
    Dim folderPath As String
    Dim fileName As String
    Dim baseName As String
    Dim dotPos As Long
    Dim lastRow As Long, r As Long
    Dim matchedRow As Long
    Dim shp As Shape
    Dim pic As Shape
    Dim cellTarget As Range
    Dim cellW As Double, cellH As Double, picW As Double, picH As Double, sc As Double
    Dim placedCount As Long, skippedCount As Long

    On Error Resume Next
    Set ws = ThisWorkbook.Sheets(SHEET_NAME)
    On Error GoTo 0
    If ws Is Nothing Then
        MsgBox "シート「" & SHEET_NAME & "」が見つかりません。", vbExclamation
        Exit Sub
    End If

    With Application.FileDialog(msoFileDialogFolderPicker)
        .Title = "写真が入っているフォルダを選択してください"
        If .Show <> -1 Then Exit Sub
        folderPath = .SelectedItems(1)
        If Right(folderPath, 1) <> "\" Then folderPath = folderPath & "\"
    End With

    lastRow = ws.Cells(ws.Rows.Count, KEY_COL).End(xlUp).Row
    placedCount = 0
    skippedCount = 0

    fileName = Dir(folderPath & "*.*")
    Do While fileName <> ""
        Select Case LCase(Right(fileName, 4))
            Case ".jpg", ".png", ".bmp", ".gif"
                baseName = fileName
                dotPos = InStrRev(baseName, ".")
                If dotPos > 0 Then baseName = Left(baseName, dotPos - 1)
                GoTo CheckMatch
            Case Else
                If LCase(Right(fileName, 5)) = ".jpeg" Then
                    baseName = Left(fileName, Len(fileName) - 5)
                    GoTo CheckMatch
                End If
        End Select
        GoTo NextFile

CheckMatch:
        matchedRow = 0
        For r = 2 To lastRow
            If Trim(CStr(ws.Cells(r, KEY_COL).Value)) = Trim(baseName) Then
                matchedRow = r
                Exit For
            End If
        Next r

        If matchedRow = 0 Then
            skippedCount = skippedCount + 1
        Else
            ' その行のC列に、すでに写真があれば削除する
            For Each shp In ws.Shapes
                If Not Intersect(shp.TopLeftCell, ws.Cells(matchedRow, PIC_COL)) Is Nothing Then
                    shp.Delete
                End If
            Next shp

            Set cellTarget = ws.Cells(matchedRow, PIC_COL)
            Set pic = ws.Shapes.AddPicture(folderPath & fileName, False, True, _
                                           cellTarget.Left, cellTarget.Top, -1, -1)

            ' セルの大きさに収まるよう、縦横比を保ったまま縮小して中央に置く
            cellW = cellTarget.Width - 4
            cellH = cellTarget.Height - 4
            picW = pic.Width
            picH = pic.Height
            sc = WorksheetFunction.Min(cellW / picW, cellH / picH)
            If sc < 1 Then
                pic.Width = picW * sc
                pic.Height = picH * sc
            End If
            pic.Left = cellTarget.Left + (cellTarget.Width - pic.Width) / 2
            pic.Top = cellTarget.Top + (cellTarget.Height - pic.Height) / 2
            pic.Placement = xlMoveAndSize

            placedCount = placedCount + 1
        End If

NextFile:
        fileName = Dir
    Loop

    MsgBox "配置しました: " & placedCount & " 件" & vbCrLf & _
           "一致する什器Noが見つからなかったファイル: " & skippedCount & " 件", vbInformation
End Sub
