;;; ============================================================
;;;  QS - クイック選択拡張 (v13: 検索範囲をUIから廃止しコマンドライン指定へ変更・v10: ロック・凍結レイヤーのオブジェクトを除外・[7]回転角度を追加・フルスクリーン展開時のマウスホイール対応・項目欄をフルスクリーン展開・フッター/ヘッダー領域縮小・右クリックで決定・UI拡大・文字サイズ12・単一項目の表示不具合修正)
;;;
;;;  必要なファイルは QS.lsp 1本だけです。コマンド名は "QS" です。
;;;  PowerShell(WPF)のダイアログ用スクリプトはこのファイルの中に
;;;  文字列として埋め込まれており、実行のたびに一時フォルダへ
;;;  自動的に書き出されます(qse-write-ps1-script関数)。
;;;
;;;  v13での変更点:
;;;   ・ダイアログから「検索範囲」欄(ラジオボタン)を削除。範囲の指定は
;;;     ダイアログで「適用」したあと、コマンドラインで行う方式に変更
;;;       Enter        : AutoCAD標準の選択(窓=青/交差=緑の標準表示、クリックも可、繰り返し可)
;;;       何も選ばずEnter: 図面全体(現在の空間)から検索
;;;       P            : 閉じたポリラインを選び、その内側を選択
;;;   ・前回値の記憶(QS_LastRange)は廃止
;;;
;;;  v12での変更点:
;;;   ・[5]ブロック名の項目欄にも検索欄を追加(ブロック名の一部を入力すると
;;;     一覧が絞り込まれる。大文字小文字は区別しない)
;;;
;;;  v11での変更点(見直し):
;;;   ・画層名・線種名・ブロック名・文字列内容に # @ . * ? , [ ] などが
;;;     含まれるとssgetのワイルドカードとして解釈され、意図しない
;;;     オブジェクトまで選択されていたのを修正(` でエスケープ)
;;;   ・ダイナミックブロックが "*U123" のような匿名名で表示され、選ぶと
;;;     全ブロックがヒットしていたのを修正(実効名 EffectiveName で集計・判定)
;;;   ・ブロック名の条件がHATCHのパターン名(グループコード2)にも
;;;     ヒットしていたのを修正(ブロック参照のみを対象に判定)
;;;   ・属性(ATTRIB)の値やオブジェクトタイプ「属性」を選んでも、ssgetは
;;;     属性を直接選べないため常に0件だったのを修正。属性値に一致する
;;;     ブロック参照を選択するようにした
;;;   ・回転角度は丸めた値の完全一致ではなく ±0.005° の範囲で判定する
;;;     (0°と360°付近も同一視)。円弧(ARC)のグループコード50は
;;;     「開始角度」なので回転角度の一覧・条件から除外
;;;   ・「図面全体から検索」で他のレイアウトのオブジェクトまで
;;;     選択されていたのを修正(現在の空間のみ)
;;;   ・検索欄に文字を入力すると、それまでの選択が消えていたのを修正
;;;     (絞り込みで隠れた項目の選択も保持される)
;;;   ・選択中にEscで中断したとき一時ファイルが残らないようにした
;;;
;;;  v10での変更点:
;;;   ・ロックされたレイヤー・凍結されたレイヤーのオブジェクトを除外する。
;;;     基準オブジェクトの中にこれらが含まれていても、条件設定(各項目欄の
;;;     一覧)には反映されない。また最終的な選択時も、「図面全体から検索」
;;;     (ssget "X")はロック・凍結を無視してヒットしてしまうため、
;;;     検索範囲によらず統一して除外するようにした
;;;
;;;  v9での変更点:
;;;   ・項目欄に [7] 回転角度 を追加。DXFグループコード50(ラジアン)を
;;;     度数に変換し、0〜360度の範囲に正規化のうえ小数点2桁で丸めて
;;;     件数付きで一覧表示する(例: "90.00° (3件)")。
;;;     ※ 丸めた度数をそのままssgetフィルタの値(ラジアンへ逆変換)として
;;;     使うため、表示上は同じ角度でも実際のデータがごく僅かに異なる
;;;     (例: スクリプト等で直接ラジアン値を与えて作成された場合)と、
;;;     一覧上は同じに見えてもすべては選択されないことがある。
;;;     「90」「180」など、コマンドで角度を直接入力して回転させた
;;;     オブジェクト同士であれば通常は同じ値になり問題ない。
;;;
;;;  v8での変更点:
;;;   ・ダイアログのウィンドウを大きく(幅1000→1200、一覧の高さ108→150 など)し、
;;;     UIの文字サイズは12に統一し、左右の項目欄の間隔も広げた(16→32)。
;;;     検索欄は上下の余白を詰めて低くした。
;;;   ・(v13で廃止) 「検索範囲」は前回選んだ値で起動する。値はAutoCAD環境変数
;;;     (getenv/setenv の QS_LastRange)に保存するので、図面を替えたり
;;;     AutoCADを再起動しても引き継がれる。適用(OK)したときだけ更新される。
;;;   ・ダイアログ内(検索欄を除く)を右クリックすると、「適用」ボタンを
;;;     押したのと同じ扱いになる。検索欄の上で右クリックした場合は、
;;;     従来通り右クリックメニュー(コピー/貼り付け)を優先する。
;;;   ・タイトルバー(▼アイコンとタイトル文字の部分)の高さ・アイコンの
;;;     大きさを縮小(高さ56→40、▼アイコン16→13、閉じるボタン52x56→40x40)
;;;   ・フッター(検索範囲欄・下部ボタン)の余白を詰めて全体を縮小
;;;   ・項目欄のタイトルをクリックしたときの展開を、欄内で少し大きくする
;;;     方式から、ダイアログ全体を使って表示する方式(フルスクリーン風)に変更。
;;;     同じ場所をもう一度クリックすると元の一覧表示に戻る。
;;;   ・フルスクリーン表示中、一覧の高さを「自動(NaN)」ではなく具体的な
;;;     ピクセル数で指定するよう変更。ウィンドウの高さはダイアログの内容量に
;;;     合わせて自動調整される(SizeToContent)ため、高さを自動のままにすると
;;;     一覧の枠がスクロールバーを持たずに際限なく伸びてしまい、マウスホイールで
;;;     のスクロールが効かなくなる。あらかじめ計算した高さを与えることで、
;;;     項目数が多いときは一覧の中でスクロール(マウスホイール対応)するようにした。
;;;     小さい画面ではみ出さないよう、作業領域に合わせて幅・高さの上限を自動調整
;;;   ・ブロック名/文字列内容が1種類だけのとき、先頭1文字しか表示
;;;     されない不具合を修正(PowerShell側Get-Sectionの戻り値が1要素だと
;;;     文字列に展開されてしまうため、カンマ演算子で包んでリストのまま返す)
;;;
;;;  v5での変更点:
;;;   ・コマンド名を QSE → QS に変更
;;;   ・オブジェクトタイプを日本語ラベルで表示
;;;     (内部的なDXF実体名とは別に表示用ラベルを用意し、
;;;      "DXF名|日本語ラベル" の形でPowerShell側へ渡す。
;;;      選択結果はTagとして保持しているDXF名の方が返るため、
;;;      LISP側での逆変換は不要)
;;;   ・各項目欄(カード)に色を付けて見分けやすく
;;;
;;;  仕組み:
;;;   1) 選択オブジェクトからプロパティを収集
;;;   2) データをテキストファイルに書き出し (###見出し### 区切り形式)
;;;   3) 埋め込みPowerShellスクリプトを一時ファイルへ書き出す
;;;   4) WScript.Shell経由でPowerShellを同期起動しWPFダイアログを表示
;;;   5) ユーザーの選択結果を同形式のテキストファイルから読み込み
;;;   6) 従来通りssgetフィルタを構築して選択を実行
;;;   7) 一時ファイル(データ・PS1とも)を削除して後片付け
;;;
;;;  文字コード: PowerShell側は -Encoding Default (システムANSI)
;;;  を使用しており、AutoLISPの標準open/write-line/read-lineと
;;;  揃えています。Windows PowerShell 5.1 (powershell.exe) を
;;;  前提としています(PowerShell 7 / pwsh.exeではありません)。
;;; ============================================================

;; ---------- ファイル入出力ヘルパー ----------

(defun qse-write-section (fn header lst)
  (write-line (strcat "###" header "###") fn)
  (foreach v lst (write-line v fn))
)

;; PowerShell側が書き出した結果ファイルを読み込み、
;; (type layer color ltype blk txt angle result errormsg) の
;; 9要素リストを返す
(defun qse-read-output (path / fn line cur
                            outType outLayer outColor outLtype outBlk outTxt outAngle
                            outResult outErr)
  (setq outType '() outLayer '() outColor '() outLtype '() outBlk '() outTxt '() outAngle '()
        outResult nil outErr nil cur nil)
  (setq fn (open path "r"))
  (if fn
    (progn
      (while (setq line (read-line fn))
        (cond
          ((= line "###TYPE###")     (setq cur 'TYPE))
          ((= line "###LAYER###")    (setq cur 'LAYER))
          ((= line "###COLOR###")    (setq cur 'COLOR))
          ((= line "###LTYPE###")    (setq cur 'LTYPE))
          ((= line "###BLOCK###")    (setq cur 'BLOCK))
          ((= line "###TEXT###")     (setq cur 'TEXT))
          ((= line "###ANGLE###")    (setq cur 'ANGLE))
          ((= line "###RESULT###")   (setq cur 'RESULT))
          ((= line "###ERRORMSG###") (setq cur 'ERRORMSG))
          ((/= line "")
           (cond
             ((eq cur 'TYPE)     (setq outType   (cons line outType)))
             ((eq cur 'LAYER)    (setq outLayer  (cons line outLayer)))
             ((eq cur 'COLOR)    (setq outColor  (cons line outColor)))
             ((eq cur 'LTYPE)    (setq outLtype  (cons line outLtype)))
             ((eq cur 'BLOCK)    (setq outBlk    (cons line outBlk)))
             ((eq cur 'TEXT)     (setq outTxt    (cons line outTxt)))
             ((eq cur 'ANGLE)    (setq outAngle  (cons line outAngle)))
             ((eq cur 'RESULT)   (setq outResult line))
             ((eq cur 'ERRORMSG) (setq outErr    line))
           )
          )
        )
      )
      (close fn)
    )
  )
  (list (reverse outType) (reverse outLayer) (reverse outColor) (reverse outLtype)
        (reverse outBlk) (reverse outTxt) (reverse outAngle) outResult outErr)
)

;; ---------- 埋め込みPowerShell(WPF)スクリプトの書き出し ----------
(defun qse-write-ps1-script (path / fn)
  (setq fn (open path "w"))
  (write-line "[CmdletBinding()]" fn)
  (write-line "param(" fn)
  (write-line "    [Parameter(Mandatory=$true)][string]$InputPath," fn)
  (write-line "    [Parameter(Mandatory=$true)][string]$OutputPath" fn)
  (write-line ")" fn)
  (write-line "" fn)
  (write-line "Add-Type -AssemblyName PresentationFramework" fn)
  (write-line "Add-Type -AssemblyName PresentationCore" fn)
  (write-line "Add-Type -AssemblyName WindowsBase" fn)
  (write-line "" fn)
  (write-line "# ============================================================" fn)
  (write-line "#  入出力ヘルパー" fn)
  (write-line "#  文字コードは Windows PowerShell 5.1 (powershell.exe) の" fn)
  (write-line "#  -Encoding Default (システムのANSIコードページ) を使用。" fn)
  (write-line "#  AutoLISP側の open/write-line/read-line と揃えるため。" fn)
  (write-line "# ============================================================" fn)
  (write-line "" fn)
  (write-line "function Read-Sections {" fn)
  (write-line "    param([string]$Path)" fn)
  (write-line "    $sections = @{}" fn)
  (write-line "    $current = $null" fn)
  (write-line "    if (Test-Path -LiteralPath $Path) {" fn)
  (write-line "        $content = Get-Content -LiteralPath $Path -Encoding Default" fn)
  (write-line "        foreach ($line in $content) {" fn)
  (write-line "            if ($line -match '^###(.+)###$') {" fn)
  (write-line "                $current = $Matches[1]" fn)
  (write-line "                $sections[$current] = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "            }" fn)
  (write-line "            elseif ($null -ne $current -and $line -ne '') {" fn)
  (write-line "                $sections[$current].Add($line)" fn)
  (write-line "            }" fn)
  (write-line "        }" fn)
  (write-line "    }" fn)
  (write-line "    return $sections" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "function Get-Section {" fn)
  (write-line "    param($Data, [string]$Key)" fn)
  (write-line "    # 戻り値はカンマ演算子で包む。包まないと、要素が1個のときPowerShellが" fn)
  (write-line "    # リストを展開して単なる文字列として返してしまい、呼び出し側の" fn)
  (write-line "    # $list[0] が「先頭1文字」になってしまう(要素0個のときも同様に消える)" fn)
  (write-line "    if ($Data.ContainsKey($Key)) { return ,$Data[$Key] }" fn)
  (write-line "    return ,(New-Object System.Collections.Generic.List[string])" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "function Write-ResultFile {" fn)
  (write-line "    param(" fn)
  (write-line "        [string]$Path," fn)
  (write-line "        [string]$Status," fn)
  (write-line "        [string[]]$SelType," fn)
  (write-line "        [string[]]$SelLayer," fn)
  (write-line "        [string[]]$SelColor," fn)
  (write-line "        [string[]]$SelLtype," fn)
  (write-line "        [string[]]$SelBlk," fn)
  (write-line "        [string[]]$SelTxt," fn)
  (write-line "        [string[]]$SelAngle," fn)
  (write-line "        [string]$ErrorMsg" fn)
  (write-line "    )" fn)
  (write-line "    $lines = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    $lines.Add(\"###TYPE###\")" fn)
  (write-line "    if ($SelType)  { foreach ($v in $SelType)  { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###LAYER###\")" fn)
  (write-line "    if ($SelLayer) { foreach ($v in $SelLayer) { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###COLOR###\")" fn)
  (write-line "    if ($SelColor) { foreach ($v in $SelColor) { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###LTYPE###\")" fn)
  (write-line "    if ($SelLtype) { foreach ($v in $SelLtype) { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###BLOCK###\")" fn)
  (write-line "    if ($SelBlk)   { foreach ($v in $SelBlk)   { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###TEXT###\")" fn)
  (write-line "    if ($SelTxt)   { foreach ($v in $SelTxt)   { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###ANGLE###\")" fn)
  (write-line "    if ($SelAngle) { foreach ($v in $SelAngle) { $lines.Add($v) } }" fn)
  (write-line "    $lines.Add(\"###RESULT###\")" fn)
  (write-line "    $lines.Add($Status)" fn)
  (write-line "    $lines.Add(\"###ERRORMSG###\")" fn)
  (write-line "    if ($ErrorMsg) { $lines.Add($ErrorMsg) }" fn)
  (write-line "" fn)
  (write-line "    [System.IO.File]::WriteAllLines($Path, $lines, [System.Text.Encoding]::Default)" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "try {" fn)
  (write-line "" fn)
  (write-line "    $data = Read-Sections -Path $InputPath" fn)
  (write-line "    $typeRawList = Get-Section $data 'TYPE'" fn)
  (write-line "    $layerList   = Get-Section $data 'LAYER'" fn)
  (write-line "    $colorList   = Get-Section $data 'COLOR'" fn)
  (write-line "    $ltypeList   = Get-Section $data 'LTYPE'" fn)
  (write-line "    $blkList     = Get-Section $data 'BLOCK'" fn)
  (write-line "    $txtList     = Get-Section $data 'TEXT'" fn)
  (write-line "    $blkCountList = Get-Section $data 'BLOCKCOUNT'" fn)
  (write-line "    $txtCountList = Get-Section $data 'TEXTCOUNT'" fn)
  (write-line "    $angleList      = Get-Section $data 'ANGLE'" fn)
  (write-line "    $angleCountList = Get-Section $data 'ANGLECOUNT'" fn)
  (write-line "" fn)
  (write-line "    # オブジェクトタイプは \"DXFコード|表示ラベル\" の形式で来るので分割する" fn)
  (write-line "    # (DXFのエンティティ種別名は | を含み得ないため区切り文字として安全)" fn)
  (write-line "    $typeValues   = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    $typeDisplays = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    foreach ($line in $typeRawList) {" fn)
  (write-line "        $sep = $line.IndexOf('|')" fn)
  (write-line "        if ($sep -ge 0) {" fn)
  (write-line "            $typeValues.Add($line.Substring(0, $sep))" fn)
  (write-line "            $typeDisplays.Add($line.Substring($sep + 1))" fn)
  (write-line "        } else {" fn)
  (write-line "            $typeValues.Add($line)" fn)
  (write-line "            $typeDisplays.Add($line)" fn)
  (write-line "        }" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    # ブロック名・文字列内容は、同じ並び順の件数リスト(BLOCKCOUNT/TEXTCOUNT)" fn)
  (write-line "    # と組み合わせて \"値 (n件)\" の表示ラベルを作る(値自体は変更しない)" fn)
  (write-line "    $blkDisplays = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    for ($idx = 0; $idx -lt $blkList.Count; $idx++) {" fn)
  (write-line "        $cnt = '1'" fn)
  (write-line "        if ($idx -lt $blkCountList.Count) { $cnt = $blkCountList[$idx] }" fn)
  (write-line "        $blkDisplays.Add(\"$($blkList[$idx]) ($($cnt)件)\")" fn)
  (write-line "    }" fn)
  (write-line "    $txtDisplays = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    for ($idx = 0; $idx -lt $txtList.Count; $idx++) {" fn)
  (write-line "        $cnt = '1'" fn)
  (write-line "        if ($idx -lt $txtCountList.Count) { $cnt = $txtCountList[$idx] }" fn)
  (write-line "        $txtDisplays.Add(\"$($txtList[$idx]) ($($cnt)件)\")" fn)
  (write-line "    }" fn)
  (write-line "    $angleDisplays = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "    for ($idx = 0; $idx -lt $angleList.Count; $idx++) {" fn)
  (write-line "        $cnt = '1'" fn)
  (write-line "        if ($idx -lt $angleCountList.Count) { $cnt = $angleCountList[$idx] }" fn)
  (write-line "        $angleDisplays.Add(\"$($angleList[$idx])\" + [char]0xB0 + \" ($($cnt)件)\")" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    # ============================================================" fn)
  (write-line "    #  ウィンドウの外枠 (XAML)" fn)
  (write-line "    # ============================================================" fn)
  (write-line "    $xaml = @'" fn)
  (write-line "<Window" fn)
  (write-line "    xmlns=\"http://schemas.microsoft.com/winfx/2006/xaml/presentation\"" fn)
  (write-line "    xmlns:x=\"http://schemas.microsoft.com/winfx/2006/xaml\"" fn)
  (write-line "    Title=\"QS\" Width=\"1200\"" fn)
  (write-line "    SizeToContent=\"Height\" MaxHeight=\"1000\"" fn)
  (write-line "    WindowStartupLocation=\"CenterScreen\"" fn)
  (write-line "    WindowStyle=\"None\"" fn)
  (write-line "    AllowsTransparency=\"True\"" fn)
  (write-line "    Background=\"Transparent\"" fn)
  (write-line "    ResizeMode=\"NoResize\">" fn)
  (write-line "    <Window.Resources>" fn)
  (write-line "        <Style x:Key=\"PrimaryButton\" TargetType=\"Button\">" fn)
  (write-line "            <Setter Property=\"Background\" Value=\"#1F5FA8\"/>" fn)
  (write-line "            <Setter Property=\"Foreground\" Value=\"White\"/>" fn)
  (write-line "            <Setter Property=\"FontWeight\" Value=\"Bold\"/>" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "            <Setter Property=\"Padding\" Value=\"18,8\"/>" fn)
  (write-line "            <Setter Property=\"BorderThickness\" Value=\"0\"/>" fn)
  (write-line "            <Setter Property=\"Cursor\" Value=\"Hand\"/>" fn)
  (write-line "            <Setter Property=\"Template\">" fn)
  (write-line "                <Setter.Value>" fn)
  (write-line "                    <ControlTemplate TargetType=\"Button\">" fn)
  (write-line "                        <Border x:Name=\"Bd\" Background=\"{TemplateBinding Background}\" CornerRadius=\"6\" Padding=\"{TemplateBinding Padding}\">" fn)
  (write-line "                            <ContentPresenter HorizontalAlignment=\"Center\" VerticalAlignment=\"Center\"/>" fn)
  (write-line "                        </Border>" fn)
  (write-line "                        <ControlTemplate.Triggers>" fn)
  (write-line "                            <Trigger Property=\"IsMouseOver\" Value=\"True\">" fn)
  (write-line "                                <Setter TargetName=\"Bd\" Property=\"Background\" Value=\"#18497F\"/>" fn)
  (write-line "                            </Trigger>" fn)
  (write-line "                        </ControlTemplate.Triggers>" fn)
  (write-line "                    </ControlTemplate>" fn)
  (write-line "                </Setter.Value>" fn)
  (write-line "            </Setter>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style x:Key=\"SecondaryButton\" TargetType=\"Button\">" fn)
  (write-line "            <Setter Property=\"Background\" Value=\"White\"/>" fn)
  (write-line "            <Setter Property=\"Foreground\" Value=\"#333333\"/>" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "            <Setter Property=\"Padding\" Value=\"18,8\"/>" fn)
  (write-line "            <Setter Property=\"BorderBrush\" Value=\"#CCCCCC\"/>" fn)
  (write-line "            <Setter Property=\"BorderThickness\" Value=\"1\"/>" fn)
  (write-line "            <Setter Property=\"Cursor\" Value=\"Hand\"/>" fn)
  (write-line "            <Setter Property=\"Template\">" fn)
  (write-line "                <Setter.Value>" fn)
  (write-line "                    <ControlTemplate TargetType=\"Button\">" fn)
  (write-line "                        <Border x:Name=\"Bd\" Background=\"{TemplateBinding Background}\" CornerRadius=\"6\"" fn)
  (write-line "                                BorderBrush=\"{TemplateBinding BorderBrush}\" BorderThickness=\"{TemplateBinding BorderThickness}\"" fn)
  (write-line "                                Padding=\"{TemplateBinding Padding}\">" fn)
  (write-line "                            <ContentPresenter HorizontalAlignment=\"Center\" VerticalAlignment=\"Center\"/>" fn)
  (write-line "                        </Border>" fn)
  (write-line "                        <ControlTemplate.Triggers>" fn)
  (write-line "                            <Trigger Property=\"IsMouseOver\" Value=\"True\">" fn)
  (write-line "                                <Setter TargetName=\"Bd\" Property=\"Background\" Value=\"#F2F2F2\"/>" fn)
  (write-line "                            </Trigger>" fn)
  (write-line "                        </ControlTemplate.Triggers>" fn)
  (write-line "                    </ControlTemplate>" fn)
  (write-line "                </Setter.Value>" fn)
  (write-line "            </Setter>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style x:Key=\"LinkButton\" TargetType=\"Button\">" fn)
  (write-line "            <Setter Property=\"Background\" Value=\"Transparent\"/>" fn)
  (write-line "            <Setter Property=\"Foreground\" Value=\"#1F5FA8\"/>" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "            <Setter Property=\"BorderThickness\" Value=\"0\"/>" fn)
  (write-line "            <Setter Property=\"Cursor\" Value=\"Hand\"/>" fn)
  (write-line "            <Setter Property=\"Padding\" Value=\"0\"/>" fn)
  (write-line "            <Setter Property=\"Template\">" fn)
  (write-line "                <Setter.Value>" fn)
  (write-line "                    <ControlTemplate TargetType=\"Button\">" fn)
  (write-line "                        <ContentPresenter x:Name=\"Cp\"/>" fn)
  (write-line "                        <ControlTemplate.Triggers>" fn)
  (write-line "                            <Trigger Property=\"IsMouseOver\" Value=\"True\">" fn)
  (write-line "                                <Setter Property=\"TextBlock.TextDecorations\" Value=\"Underline\"/>" fn)
  (write-line "                            </Trigger>" fn)
  (write-line "                        </ControlTemplate.Triggers>" fn)
  (write-line "                    </ControlTemplate>" fn)
  (write-line "                </Setter.Value>" fn)
  (write-line "            </Setter>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style x:Key=\"MiniLink\" TargetType=\"Button\" BasedOn=\"{StaticResource LinkButton}\">" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "            <Setter Property=\"Margin\" Value=\"14,0,0,0\"/>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style x:Key=\"SearchBox\" TargetType=\"TextBox\">" fn)
  (write-line "            <Setter Property=\"Padding\" Value=\"8,4\"/>" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "            <Setter Property=\"Template\">" fn)
  (write-line "                <Setter.Value>" fn)
  (write-line "                    <ControlTemplate TargetType=\"TextBox\">" fn)
  (write-line "                        <Border x:Name=\"Bd\" Background=\"White\" CornerRadius=\"5\" BorderBrush=\"#CCCCCC\" BorderThickness=\"1\">" fn)
  (write-line "                            <ScrollViewer x:Name=\"PART_ContentHost\" Margin=\"{TemplateBinding Padding}\" VerticalAlignment=\"Center\"/>" fn)
  (write-line "                        </Border>" fn)
  (write-line "                        <ControlTemplate.Triggers>" fn)
  (write-line "                            <Trigger Property=\"IsFocused\" Value=\"True\">" fn)
  (write-line "                                <Setter TargetName=\"Bd\" Property=\"BorderBrush\" Value=\"#1F5FA8\"/>" fn)
  (write-line "                            </Trigger>" fn)
  (write-line "                        </ControlTemplate.Triggers>" fn)
  (write-line "                    </ControlTemplate>" fn)
  (write-line "                </Setter.Value>" fn)
  (write-line "            </Setter>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style TargetType=\"ListBox\">" fn)
  (write-line "            <Setter Property=\"BorderBrush\" Value=\"#E2E2E2\"/>" fn)
  (write-line "            <Setter Property=\"BorderThickness\" Value=\"1\"/>" fn)
  (write-line "            <Setter Property=\"Background\" Value=\"#FCFCFC\"/>" fn)
  (write-line "            <Setter Property=\"FontSize\" Value=\"12\"/>" fn)
  (write-line "        </Style>" fn)
  (write-line "" fn)
  (write-line "        <Style TargetType=\"ListBoxItem\">" fn)
  (write-line "            <Setter Property=\"Padding\" Value=\"6,4\"/>" fn)
  (write-line "        </Style>" fn)
  (write-line "    </Window.Resources>" fn)
  (write-line "" fn)
  (write-line "    <Border Background=\"White\" CornerRadius=\"10\" BorderBrush=\"#DDDDDD\" BorderThickness=\"1\">" fn)
  (write-line "        <Border.Effect>" fn)
  (write-line "            <DropShadowEffect BlurRadius=\"20\" ShadowDepth=\"5\" Opacity=\"0.25\" Color=\"#000000\"/>" fn)
  (write-line "        </Border.Effect>" fn)
  (write-line "        <Grid>" fn)
  (write-line "            <Grid.RowDefinitions>" fn)
  (write-line "                <RowDefinition Height=\"40\"/>" fn)
  (write-line "                <RowDefinition Height=\"*\"/>" fn)
  (write-line "            </Grid.RowDefinitions>" fn)
  (write-line "" fn)
  (write-line "            <Border x:Name=\"HeaderBar\" Grid.Row=\"0\" Background=\"#DCEFE0\" CornerRadius=\"10,10,0,0\">" fn)
  (write-line "                <Grid>" fn)
  (write-line "                    <Grid.ColumnDefinitions>" fn)
  (write-line "                        <ColumnDefinition Width=\"Auto\"/>" fn)
  (write-line "                        <ColumnDefinition Width=\"*\"/>" fn)
  (write-line "                        <ColumnDefinition Width=\"Auto\"/>" fn)
  (write-line "                    </Grid.ColumnDefinitions>" fn)
  (write-line "                    <TextBlock Grid.Column=\"0\" Text=\"&#9660;\" Foreground=\"#2F6FBD\" FontSize=\"13\"" fn)
  (write-line "                               VerticalAlignment=\"Center\" Margin=\"12,0,6,0\"/>" fn)
  (write-line "                    <TextBlock Grid.Column=\"1\" Text=\"クイック選択 - 条件設定\" FontSize=\"12\" FontWeight=\"Bold\"" fn)
  (write-line "                               Foreground=\"#2B2B2B\" VerticalAlignment=\"Center\"/>" fn)
  (write-line "                    <Button x:Name=\"BtnClose\" Grid.Column=\"2\" Content=\"&#10005;\" Width=\"40\" Height=\"40\"" fn)
  (write-line "                            Background=\"Transparent\" BorderThickness=\"0\" Foreground=\"#555555\" FontSize=\"14\"" fn)
  (write-line "                            Cursor=\"Hand\"/>" fn)
  (write-line "                </Grid>" fn)
  (write-line "            </Border>" fn)
  (write-line "" fn)
  (write-line "            <Grid Grid.Row=\"1\" Margin=\"20,16,20,14\">" fn)
  (write-line "                <Grid.RowDefinitions>" fn)
  (write-line "                    <RowDefinition Height=\"Auto\"/>" fn)
  (write-line "                    <RowDefinition Height=\"Auto\"/>" fn)
  (write-line "                    <RowDefinition Height=\"Auto\"/>" fn)
  (write-line "                </Grid.RowDefinitions>" fn)
  (write-line "" fn)
  (write-line "                <TextBlock Grid.Row=\"0\" Text=\"絞り込む項目を選択してください（Ctrl+クリックで複数選択可）\"" fn)
  (write-line "                           Foreground=\"#666666\" FontSize=\"12\" Margin=\"0,0,0,12\"/>" fn)
  (write-line "" fn)
  (write-line "                <ScrollViewer x:Name=\"MainScroll\" Grid.Row=\"1\" Height=\"600\" VerticalScrollBarVisibility=\"Auto\" HorizontalScrollBarVisibility=\"Disabled\">" fn)
  (write-line "                    <Grid Margin=\"0,0,10,0\">" fn)
  (write-line "                        <Grid.ColumnDefinitions>" fn)
  (write-line "                            <ColumnDefinition Width=\"*\"/>" fn)
  (write-line "                            <ColumnDefinition Width=\"32\"/>" fn)
  (write-line "                            <ColumnDefinition Width=\"*\"/>" fn)
  (write-line "                        </Grid.ColumnDefinitions>" fn)
  (write-line "                        <StackPanel Grid.Column=\"0\" x:Name=\"ColLeft\"/>" fn)
  (write-line "                        <StackPanel Grid.Column=\"2\" x:Name=\"ColRight\"/>" fn)
  (write-line "                    </Grid>" fn)
  (write-line "                </ScrollViewer>" fn)
  (write-line "" fn)
  (write-line "                <Grid Grid.Row=\"2\" Margin=\"0,10,0,0\">" fn)
  (write-line "                    <Grid.ColumnDefinitions>" fn)
  (write-line "                        <ColumnDefinition Width=\"Auto\"/>" fn)
  (write-line "                        <ColumnDefinition Width=\"*\"/>" fn)
  (write-line "                        <ColumnDefinition Width=\"Auto\"/>" fn)
  (write-line "                        <ColumnDefinition Width=\"Auto\"/>" fn)
  (write-line "                    </Grid.ColumnDefinitions>" fn)
  (write-line "                    <StackPanel Grid.Column=\"0\" Orientation=\"Horizontal\" VerticalAlignment=\"Center\">" fn)
  (write-line "                        <Button x:Name=\"BtnSelectAllGlobal\" Content=\"すべて全選択\" Style=\"{StaticResource LinkButton}\"/>" fn)
  (write-line "                        <TextBlock Text=\"  |  \" Foreground=\"#CCCCCC\" VerticalAlignment=\"Center\"/>" fn)
  (write-line "                        <Button x:Name=\"BtnReset\" Content=\"条件を全てリセット\" Style=\"{StaticResource LinkButton}\"/>" fn)
  (write-line "                    </StackPanel>" fn)
  (write-line "                    <Button x:Name=\"BtnCancel\" Grid.Column=\"2\" Content=\"キャンセル (Esc)\" Style=\"{StaticResource SecondaryButton}\"" fn)
  (write-line "                            Margin=\"0,0,10,0\" IsCancel=\"True\"/>" fn)
  (write-line "                    <Button x:Name=\"BtnApply\" Grid.Column=\"3\" Content=\"適用 (Enter)\" Style=\"{StaticResource PrimaryButton}\"" fn)
  (write-line "                            IsDefault=\"True\"/>" fn)
  (write-line "                </Grid>" fn)
  (write-line "" fn)
  (write-line "                <!-- 項目欄をフルスクリーン風に展開するときの受け皿。" fn)
  (write-line "                     通常は非表示で、展開時だけ項目欄の中身(タイトル行/検索欄/一覧)を" fn)
  (write-line "                     ここへ移し、ダイアログ全体を使って表示する -->" fn)
  (write-line "                <Border x:Name=\"ExpandOverlay\" Grid.Row=\"1\" Background=\"White\"" fn)
  (write-line "                        CornerRadius=\"0,0,10,10\" Visibility=\"Collapsed\">" fn)
  (write-line "                    <Grid x:Name=\"OverlayGrid\" Margin=\"20,16,20,14\">" fn)
  (write-line "                        <Grid.RowDefinitions>" fn)
  (write-line "                            <RowDefinition Height=\"Auto\"/>" fn)
  (write-line "                            <RowDefinition Height=\"Auto\"/>" fn)
  (write-line "                            <RowDefinition Height=\"*\"/>" fn)
  (write-line "                        </Grid.RowDefinitions>" fn)
  (write-line "                    </Grid>" fn)
  (write-line "                </Border>" fn)
  (write-line "            </Grid>" fn)
  (write-line "        </Grid>" fn)
  (write-line "    </Border>" fn)
  (write-line "</Window>" fn)
  (write-line "'@" fn)
  (write-line "" fn)
  (write-line "    $reader = New-Object System.Xml.XmlNodeReader ([xml]$xaml)" fn)
  (write-line "    $window = [System.Windows.Markup.XamlReader]::Load($reader)" fn)
  (write-line "" fn)
  (write-line "    # 画面(作業領域)が小さい環境でもはみ出さないよう、幅・高さに上限を設ける" fn)
  (write-line "    $mainScroll = $window.FindName(\"MainScroll\")" fn)
  (write-line "    $wa = [System.Windows.SystemParameters]::WorkArea" fn)
  (write-line "    $window.Width     = [Math]::Min(1200, $wa.Width - 40)" fn)
  (write-line "    $window.MaxHeight = [Math]::Max(500, $wa.Height - 20)" fn)
  (write-line "    $mainScroll.Height = [Math]::Max(320, [Math]::Min(600, $wa.Height - 280))" fn)
  (write-line "" fn)
  (write-line "    $colLeft    = $window.FindName(\"ColLeft\")" fn)
  (write-line "    $colRight   = $window.FindName(\"ColRight\")" fn)
  (write-line "    $headerBar  = $window.FindName(\"HeaderBar\")" fn)
  (write-line "    $btnClose   = $window.FindName(\"BtnClose\")" fn)
  (write-line "    $btnReset   = $window.FindName(\"BtnReset\")" fn)
  (write-line "    $btnSelectAllGlobal = $window.FindName(\"BtnSelectAllGlobal\")" fn)
  (write-line "    $btnCancel  = $window.FindName(\"BtnCancel\")" fn)
  (write-line "    $btnApply   = $window.FindName(\"BtnApply\")" fn)
  (write-line "" fn)
  (write-line "    $expandOverlay = $window.FindName(\"ExpandOverlay\")" fn)
  (write-line "    $overlayGrid   = $window.FindName(\"OverlayGrid\")" fn)
  (write-line "" fn)
  (write-line "    $headerBar.Add_MouseLeftButtonDown({ $window.DragMove() })" fn)
  (write-line "" fn)
  (write-line "    # ============================================================" fn)
  (write-line "    #  セクションカードを動的に生成する関数" fn)
  (write-line "    # ============================================================" fn)
  (write-line "    function New-SectionCard {" fn)
  (write-line "        param(" fn)
  (write-line "            [string]$Title," fn)
  (write-line "            [System.Collections.Generic.List[string]]$Values," fn)
  (write-line "            [System.Collections.Generic.List[string]]$Displays," fn)
  (write-line "            [bool]$HasSearch," fn)
  (write-line "            [string]$AccentColor," fn)
  (write-line "            [System.Windows.ResourceDictionary]$Res," fn)
  (write-line "            [System.Windows.Controls.Border]$Overlay," fn)
  (write-line "            [System.Windows.Controls.Grid]$OverlayGrid," fn)
  (write-line "            [double]$MaxDialogHeight" fn)
  (write-line "        )" fn)
  (write-line "" fn)
  (write-line "        $card = New-Object System.Windows.Controls.Border" fn)
  (write-line "        $card.Background = \"White\"" fn)
  (write-line "        $card.BorderBrush = $AccentColor" fn)
  (write-line "        $card.BorderThickness = 1.5" fn)
  (write-line "        $card.CornerRadius = 8" fn)
  (write-line "        $card.Padding = 14" fn)
  (write-line "        $card.Margin = \"0,0,0,14\"" fn)
  (write-line "" fn)
  (write-line "        $stack = New-Object System.Windows.Controls.StackPanel" fn)
  (write-line "        $card.Child = $stack" fn)
  (write-line "" fn)
  (write-line "        $headerGrid = New-Object System.Windows.Controls.Grid" fn)
  (write-line "        foreach ($w in @(\"*\",\"Auto\",\"Auto\")) {" fn)
  (write-line "            $cd = New-Object System.Windows.Controls.ColumnDefinition" fn)
  (write-line "            $cd.Width = $w" fn)
  (write-line "            $headerGrid.ColumnDefinitions.Add($cd)" fn)
  (write-line "        }" fn)
  (write-line "" fn)
  (write-line "        # タイトル部分はクリックできる領域にして、押すと一覧(ListBox)を" fn)
  (write-line "        # 展開/折りたたみできるようにする" fn)
  (write-line "        $headerClick = New-Object System.Windows.Controls.Border" fn)
  (write-line "        $headerClick.Background = [System.Windows.Media.Brushes]::Transparent" fn)
  (write-line "        $headerClick.Cursor = \"Hand\"" fn)
  (write-line "        [System.Windows.Controls.Grid]::SetColumn($headerClick, 0)" fn)
  (write-line "" fn)
  (write-line "        $headerStack = New-Object System.Windows.Controls.StackPanel" fn)
  (write-line "        $headerStack.Orientation = \"Horizontal\"" fn)
  (write-line "        $headerClick.Child = $headerStack" fn)
  (write-line "" fn)
  (write-line "        $titleText = New-Object System.Windows.Controls.TextBlock" fn)
  (write-line "        $titleText.Text = \"[+] \" + $Title" fn)
  (write-line "        $titleText.FontWeight = \"Bold\"" fn)
  (write-line "        $titleText.FontSize = 12" fn)
  (write-line "        $titleText.Foreground = $AccentColor" fn)
  (write-line "        $titleText.VerticalAlignment = \"Center\"" fn)
  (write-line "" fn)
  (write-line "        $countText = New-Object System.Windows.Controls.TextBlock" fn)
  (write-line "        $countText.Text = \"(\" + $Values.Count + \"件)\"" fn)
  (write-line "        $countText.Foreground = \"#999999\"" fn)
  (write-line "        $countText.FontSize = 12" fn)
  (write-line "        $countText.Margin = \"6,0,0,0\"" fn)
  (write-line "        $countText.VerticalAlignment = \"Center\"" fn)
  (write-line "" fn)
  (write-line "        [void]$headerStack.Children.Add($titleText)" fn)
  (write-line "        [void]$headerStack.Children.Add($countText)" fn)
  (write-line "" fn)
  (write-line "        $btnAll = New-Object System.Windows.Controls.Button" fn)
  (write-line "        $btnAll.Content = \"全選択\"" fn)
  (write-line "        $btnAll.Style = $Res[\"MiniLink\"]" fn)
  (write-line "        [System.Windows.Controls.Grid]::SetColumn($btnAll, 1)" fn)
  (write-line "" fn)
  (write-line "        $btnNone = New-Object System.Windows.Controls.Button" fn)
  (write-line "        $btnNone.Content = \"解除\"" fn)
  (write-line "        $btnNone.Style = $Res[\"MiniLink\"]" fn)
  (write-line "        [System.Windows.Controls.Grid]::SetColumn($btnNone, 2)" fn)
  (write-line "" fn)
  (write-line "        [void]$headerGrid.Children.Add($headerClick)" fn)
  (write-line "        [void]$headerGrid.Children.Add($btnAll)" fn)
  (write-line "        [void]$headerGrid.Children.Add($btnNone)" fn)
  (write-line "        [void]$stack.Children.Add($headerGrid)" fn)
  (write-line "" fn)
  (write-line "        $searchBox = $null" fn)
  (write-line "        if ($HasSearch) {" fn)
  (write-line "            $searchBox = New-Object System.Windows.Controls.TextBox" fn)
  (write-line "            $searchBox.Style = $Res[\"SearchBox\"]" fn)
  (write-line "            $searchBox.Margin = \"0,8,0,0\"" fn)
  (write-line "            [void]$stack.Children.Add($searchBox)" fn)
  (write-line "        }" fn)
  (write-line "" fn)
  (write-line "        $listBox = New-Object System.Windows.Controls.ListBox" fn)
  (write-line "        $listBox.SelectionMode = \"Extended\"" fn)
  (write-line "        $listBox.Height = 150" fn)
  (write-line "        $listBox.Margin = \"0,8,0,0\"" fn)
  (write-line "        for ($idx = 0; $idx -lt $Values.Count; $idx++) {" fn)
  (write-line "            $lbi = New-Object System.Windows.Controls.ListBoxItem" fn)
  (write-line "            $lbi.Content = $Displays[$idx]" fn)
  (write-line "            $lbi.Tag = $Values[$idx]" fn)
  (write-line "            [void]$listBox.Items.Add($lbi)" fn)
  (write-line "        }" fn)
  (write-line "        [void]$stack.Children.Add($listBox)" fn)
  (write-line "" fn)
  (write-line "        # 選択状態は値(Tag)の集合として別に持つ。検索欄で絞り込むと一覧を" fn)
  (write-line "        # 作り直すため、ListBoxの選択だけに頼ると隠れた項目の選択が消えてしまう" fn)
  (write-line "        $state = @{ Sel = (New-Object 'System.Collections.Generic.HashSet[string]'); Rebuilding = $false }" fn)
  (write-line "        $listBox.Add_SelectionChanged({" fn)
  (write-line "            if ($state.Rebuilding) { return }" fn)
  (write-line "            foreach ($i in $_.AddedItems)   { [void]$state.Sel.Add([string]$i.Tag) }" fn)
  (write-line "            foreach ($i in $_.RemovedItems) { [void]$state.Sel.Remove([string]$i.Tag) }" fn)
  (write-line "        }.GetNewClosure())" fn)
  (write-line "" fn)
  (write-line "        # 項目欄(タイトル部分)を押すと一覧を展開/折りたたみする" fn)
  (write-line "        # (状態はListBox自身のTagプロパティに持たせる。スクリプトブロックの" fn)
  (write-line "        #  ローカル変数に頼るより確実に状態が引き継がれるため)" fn)
  (write-line "        $listBox.Tag = \"collapsed\"" fn)
  (write-line "        $headerClick.Add_MouseLeftButtonDown({" fn)
  (write-line "            if ($listBox.Tag -eq \"expanded\") {" fn)
  (write-line "                # フルスクリーン表示から、この項目欄の中の通常表示へ戻す" fn)
  (write-line "                $OverlayGrid.Children.Remove($headerGrid)" fn)
  (write-line "                if ($HasSearch) { $OverlayGrid.Children.Remove($searchBox) }" fn)
  (write-line "                $OverlayGrid.Children.Remove($listBox)" fn)
  (write-line "                $listBox.Height = 150" fn)
  (write-line "                [void]$stack.Children.Add($headerGrid)" fn)
  (write-line "                if ($HasSearch) { [void]$stack.Children.Add($searchBox) }" fn)
  (write-line "                [void]$stack.Children.Add($listBox)" fn)
  (write-line "                $titleText.Text = \"[+] \" + $Title" fn)
  (write-line "                $Overlay.Visibility = \"Collapsed\"" fn)
  (write-line "                $listBox.Tag = \"collapsed\"" fn)
  (write-line "            } else {" fn)
  (write-line "                # タイトル行/検索欄/一覧をダイアログ全体のオーバーレイへ移し、" fn)
  (write-line "                # フルスクリーン風に大きく表示する" fn)
  (write-line "                $stack.Children.Remove($headerGrid)" fn)
  (write-line "                if ($HasSearch) { $stack.Children.Remove($searchBox) }" fn)
  (write-line "                $stack.Children.Remove($listBox)" fn)
  (write-line "                [System.Windows.Controls.Grid]::SetRow($headerGrid, 0)" fn)
  (write-line "                [void]$OverlayGrid.Children.Add($headerGrid)" fn)
  (write-line "                if ($HasSearch) {" fn)
  (write-line "                    [System.Windows.Controls.Grid]::SetRow($searchBox, 1)" fn)
  (write-line "                    [void]$OverlayGrid.Children.Add($searchBox)" fn)
  (write-line "                }" fn)
  (write-line "                [System.Windows.Controls.Grid]::SetRow($listBox, 2)" fn)
  (write-line "                # ウィンドウはSizeToContentで内容に合わせて自動的に高さが決まるため、" fn)
  (write-line "                # 一覧の高さを「自動」のままにするとスクロールバーが働かず" fn)
  (write-line "                # マウスホイールでスクロールできなくなる。ここで具体的な高さを" fn)
  (write-line "                # 計算して与えることで、項目数が多いときにきちんと一覧内で" fn)
  (write-line "                # スクロールできるようにする" fn)
  (write-line "                # 展開前(オーバーレイが非表示のうち)に、表示できる領域の高さを控える。" fn)
  (write-line "                # 一覧の高さを大きく取りすぎるとウィンドウが伸びて画面下へはみ出し、" fn)
  (write-line "                # 一覧の最後の行が見切れてしまうため、この領域に収まる高さにする" fn)
  (write-line "                $rowH = $Overlay.Parent.RowDefinitions[1].ActualHeight" fn)
  (write-line "                $listBox.Height = 150" fn)
  (write-line "                [void]$OverlayGrid.Children.Add($listBox)" fn)
  (write-line "                $Overlay.Visibility = \"Visible\"" fn)
  (write-line "                $OverlayGrid.UpdateLayout()" fn)
  (write-line "                $used = 30 + 8 + 4 + $headerGrid.ActualHeight + $headerGrid.Margin.Top + $headerGrid.Margin.Bottom" fn)
  (write-line "                if ($HasSearch) { $used += $searchBox.ActualHeight + $searchBox.Margin.Top + $searchBox.Margin.Bottom }" fn)
  (write-line "                $listBox.Height = [Math]::Max(120, $rowH - $used)" fn)
  (write-line "                $titleText.Text = \"[-] \" + $Title" fn)
  (write-line "                $Overlay.Visibility = \"Visible\"" fn)
  (write-line "                $listBox.Tag = \"expanded\"" fn)
  (write-line "            }" fn)
  (write-line "            $_.Handled = $true" fn)
  (write-line "        }.GetNewClosure())" fn)
  (write-line "" fn)
  (write-line "        $btnAll.Add_Click({" fn)
  (write-line "            $listBox.SelectedItems.Clear()" fn)
  (write-line "            foreach ($i in $listBox.Items) { [void]$listBox.SelectedItems.Add($i) }" fn)
  (write-line "        }.GetNewClosure())" fn)
  (write-line "" fn)
  (write-line "        $btnNone.Add_Click({" fn)
  (write-line "            $listBox.SelectedItems.Clear()" fn)
  (write-line "            $state.Sel.Clear()" fn)
  (write-line "        }.GetNewClosure())" fn)
  (write-line "" fn)
  (write-line "        if ($HasSearch) {" fn)
  (write-line "            $searchBox.Add_TextChanged({" fn)
  (write-line "                $f = $searchBox.Text" fn)
  (write-line "                $state.Rebuilding = $true" fn)
  (write-line "                $listBox.Items.Clear()" fn)
  (write-line "                for ($idx = 0; $idx -lt $Values.Count; $idx++) {" fn)
  (write-line "                    $disp = $Displays[$idx]" fn)
  (write-line "                    if ([string]::IsNullOrEmpty($f) -or $disp.ToLower().Contains($f.ToLower())) {" fn)
  (write-line "                        $lbi = New-Object System.Windows.Controls.ListBoxItem" fn)
  (write-line "                        $lbi.Content = $disp" fn)
  (write-line "                        $lbi.Tag = $Values[$idx]" fn)
  (write-line "                        [void]$listBox.Items.Add($lbi)" fn)
  (write-line "                        if ($state.Sel.Contains([string]$Values[$idx])) { [void]$listBox.SelectedItems.Add($lbi) }" fn)
  (write-line "                    }" fn)
  (write-line "                }" fn)
  (write-line "                $state.Rebuilding = $false" fn)
  (write-line "            }.GetNewClosure())" fn)
  (write-line "" fn)
  (write-line "            $searchBox.Add_PreviewKeyDown({" fn)
  (write-line "                if ($_.Key -eq [System.Windows.Input.Key]::Return) { $_.Handled = $true }" fn)
  (write-line "            }.GetNewClosure())" fn)
  (write-line "        }" fn)
  (write-line "" fn)
  (write-line "        return [PSCustomObject]@{ Card = $card; ListBox = $listBox; Values = $Values; Displays = $Displays; State = $state }" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    $cardType  = New-SectionCard -Title \"[1] オブジェクトタイプ\"     -Values $typeValues -Displays $typeDisplays -HasSearch $false -AccentColor \"#2F6FBD\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "    $cardLayer = New-SectionCard -Title \"[2] 画層（レイヤー）\"       -Values $layerList  -Displays $layerList    -HasSearch $true  -AccentColor \"#2E9E6C\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "    $cardColor = New-SectionCard -Title \"[3] 色コード\"               -Values $colorList  -Displays $colorList    -HasSearch $false -AccentColor \"#D98527\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "    $cardLtype = New-SectionCard -Title \"[4] 線種\"                   -Values $ltypeList  -Displays $ltypeList    -HasSearch $false -AccentColor \"#7B5EA7\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "" fn)
  (write-line "    [void]$colLeft.Children.Add($cardType.Card)" fn)
  (write-line "    [void]$colLeft.Children.Add($cardLayer.Card)" fn)
  (write-line "    [void]$colLeft.Children.Add($cardColor.Card)" fn)
  (write-line "    [void]$colRight.Children.Add($cardLtype.Card)" fn)
  (write-line "" fn)
  (write-line "    $cardBlk = $null" fn)
  (write-line "    if ($blkList.Count -gt 0) {" fn)
  (write-line "        $cardBlk = New-SectionCard -Title \"[5] ブロック名\" -Values $blkList -Displays $blkDisplays -HasSearch $true -AccentColor \"#1D8A99\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "        [void]$colRight.Children.Add($cardBlk.Card)" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    $cardTxt = $null" fn)
  (write-line "    if ($txtList.Count -gt 0) {" fn)
  (write-line "        $cardTxt = New-SectionCard -Title \"[6] 文字列内容\" -Values $txtList -Displays $txtDisplays -HasSearch $true -AccentColor \"#C24E6B\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "        [void]$colRight.Children.Add($cardTxt.Card)" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    $cardAngle = $null" fn)
  (write-line "    if ($angleList.Count -gt 0) {" fn)
  (write-line "        $cardAngle = New-SectionCard -Title \"[7] 回転角度\" -Values $angleList -Displays $angleDisplays -HasSearch $false -AccentColor \"#5C6BC0\" -Res $window.Resources -Overlay $expandOverlay -OverlayGrid $overlayGrid -MaxDialogHeight $window.MaxHeight" fn)
  (write-line "        [void]$colRight.Children.Add($cardAngle.Card)" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    $script:written = $false" fn)
  (write-line "" fn)
  (write-line "    # 選択済みの値は、表示用ラベルではなく実際の値(Tag)の集合から取り出す" fn)
  (write-line "    # (検索欄の絞り込みで隠れている項目の選択も含める)" fn)
  (write-line "    function Get-TagValues {" fn)
  (write-line "        param($CardObj)" fn)
  (write-line "        $out = New-Object System.Collections.Generic.List[string]" fn)
  (write-line "        if ($CardObj) { foreach ($v in $CardObj.State.Sel) { $out.Add($v) } }" fn)
  (write-line "        return $out" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    function Save-Result {" fn)
  (write-line "        param([string]$Status)" fn)
  (write-line "        $selBlk   = @(Get-TagValues $cardBlk)" fn)
  (write-line "        $selTxt   = @(Get-TagValues $cardTxt)" fn)
  (write-line "        $selAngle = @(Get-TagValues $cardAngle)" fn)
  (write-line "        Write-ResultFile -Path $OutputPath -Status $Status `" fn)
  (write-line "            -SelType  @(Get-TagValues $cardType) `" fn)
  (write-line "            -SelLayer @(Get-TagValues $cardLayer) `" fn)
  (write-line "            -SelColor @(Get-TagValues $cardColor) `" fn)
  (write-line "            -SelLtype @(Get-TagValues $cardLtype) `" fn)
  (write-line "            -SelBlk   $selBlk `" fn)
  (write-line "            -SelTxt   $selTxt `" fn)
  (write-line "            -SelAngle $selAngle `" fn)
  (write-line "            -ErrorMsg $null" fn)
  (write-line "        $script:written = $true" fn)
  (write-line "    }" fn)
  (write-line "" fn)
  (write-line "    $btnApply.Add_Click({ Save-Result \"OK\"; $window.Close() })" fn)
  (write-line "    $btnCancel.Add_Click({ Save-Result \"CANCEL\"; $window.Close() })" fn)
  (write-line "" fn)
  (write-line "    # 検索欄(TextBox)の上以外を右クリックしたときは「適用」を押したのと同じ扱いにする。" fn)
  (write-line "    # 検索欄の上では、従来通りの右クリックメニュー(コピー/貼り付け)を優先する。" fn)
  (write-line "    function Test-InsideTextBox($obj) {" fn)
  (write-line "        $cur = $obj" fn)
  (write-line "        while ($cur -ne $null) {" fn)
  (write-line "            if ($cur -is [System.Windows.Controls.TextBox]) { return $true }" fn)
  (write-line "            $cur = [System.Windows.Media.VisualTreeHelper]::GetParent($cur)" fn)
  (write-line "        }" fn)
  (write-line "        return $false" fn)
  (write-line "    }" fn)
  (write-line "    $window.Add_PreviewMouseRightButtonUp({" fn)
  (write-line "        param($s, $e)" fn)
  (write-line "        if (Test-InsideTextBox $e.OriginalSource) { return }" fn)
  (write-line "        $e.Handled = $true" fn)
  (write-line "        Save-Result \"OK\"" fn)
  (write-line "        $window.Close()" fn)
  (write-line "    })" fn)
  (write-line "    $btnClose.Add_Click({ Save-Result \"CANCEL\"; $window.Close() })" fn)
  (write-line "" fn)
  (write-line "    $btnReset.Add_Click({" fn)
  (write-line "        foreach ($c in @($cardType, $cardLayer, $cardColor, $cardLtype, $cardBlk, $cardTxt, $cardAngle)) {" fn)
  (write-line "            if ($c) { $c.ListBox.SelectedItems.Clear(); $c.State.Sel.Clear() }" fn)
  (write-line "        }" fn)
  (write-line "    })" fn)
  (write-line "" fn)
  (write-line "    $btnSelectAllGlobal.Add_Click({" fn)
  (write-line "        foreach ($c in @($cardType, $cardLayer, $cardColor, $cardLtype, $cardBlk, $cardTxt, $cardAngle)) {" fn)
  (write-line "            if ($c) {" fn)
  (write-line "                $c.ListBox.SelectedItems.Clear()" fn)
  (write-line "                foreach ($i in $c.ListBox.Items) { [void]$c.ListBox.SelectedItems.Add($i) }" fn)
  (write-line "            }" fn)
  (write-line "        }" fn)
  (write-line "    })" fn)
  (write-line "" fn)
  (write-line "    $window.Add_Closing({" fn)
  (write-line "        if (-not $script:written) { Save-Result \"CANCEL\" }" fn)
  (write-line "    })" fn)
  (write-line "" fn)
  (write-line "    [void]$window.ShowDialog()" fn)
  (write-line "" fn)
  (write-line "}" fn)
  (write-line "catch {" fn)
  (write-line "    Write-ResultFile -Path $OutputPath -Status \"ERROR\" `" fn)
  (write-line "        -SelType @() -SelLayer @() -SelColor @() -SelLtype @() -SelBlk @() -SelTxt @() -SelAngle @() `" fn)
  (write-line "        -ErrorMsg $_.Exception.Message" fn)
  (write-line "}" fn)
  (close fn)
);; ---------- オブジェクトタイプ 日本語表示テーブル ----------
;; DXF実体名(グループコード0) → 日本語表示ラベル
;; 表にない実体名はそのまま(英語)で表示されるだけなのでフィルタ動作に
;; 影響はありません。
(setq *qse-type-table*
  '(
    ("LINE"          . "線分")
    ("CIRCLE"        . "円")
    ("ARC"           . "円弧")
    ("ELLIPSE"       . "楕円")
    ("LWPOLYLINE"    . "ポリライン")
    ("POLYLINE"      . "2Dポリライン")
    ("POINT"         . "点")
    ("TEXT"          . "文字列")
    ("MTEXT"         . "マルチテキスト")
    ("INSERT"        . "ブロック参照")
    ("DIMENSION"     . "寸法")
    ("LEADER"        . "引出線")
    ("MULTILEADER"   . "マルチ引出線")
    ("HATCH"         . "ハッチング")
    ("SOLID"         . "2Dソリッド")
    ("3DFACE"        . "3D面")
    ("3DSOLID"       . "3Dソリッド")
    ("SPLINE"        . "スプライン")
    ("XLINE"         . "構築線")
    ("RAY"           . "レイ")
    ("REGION"        . "領域")
    ("WIPEOUT"       . "ワイプアウト")
    ("ACAD_TABLE"    . "テーブル")
    ("VIEWPORT"      . "ビューポート")
    ("ATTDEF"        . "属性定義")
    ("ATTRIB"        . "属性")
    ("SHAPE"         . "シェイプ")
    ("TOLERANCE"     . "公差")
    ("IMAGE"         . "イメージ")
    ("HELIX"         . "ヘリックス")
    ("LIGHT"         . "ライト")
    ("OLE2FRAME"     . "OLEオブジェクト")
    ("PDFUNDERLAY"   . "PDFアンダーレイ")
    ("DWFUNDERLAY"   . "DWFアンダーレイ")
    ("DGNUNDERLAY"   . "DGNアンダーレイ")
  )
)

;; DXF実体名から日本語ラベルを引く(表になければそのまま返す)
(defun qse-type-label (dxfName / pair)
  (setq pair (assoc dxfName *qse-type-table*))
  (if pair (cdr pair) dxfName)
)

;; ---------- 件数集計ヘルパー (v6で追加) ----------
;; alist は ((値 . 件数) ...) の形。keyが既にあれば+1、なければ新規追加。
(defun qse-count-inc (alist key / pair)
  (setq pair (assoc key alist))
  (if pair
    (subst (cons key (1+ (cdr pair))) pair alist)
    (append alist (list (cons key 1)))
  )
)

;; WScript.Shellでpowershell.exeを同期起動する(完了まで待機)
(defun qse-run-ps1 (ps1Path inPath outPath / wsh cmd)
  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq cmd (strcat
              "powershell.exe -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File "
              "\"" ps1Path "\" -InputPath \"" inPath "\" -OutputPath \"" outPath "\""
            ))
  (vlax-invoke-method wsh 'Run cmd 0 :vlax-true)
  (vlax-release-object wsh)
  (princ)
)

;; 指定したレイヤーがロックまたは凍結されているか判定する
;; (LAYERテーブルのグループコード70: bit1=凍結、bit4=ロック)
(defun qse-layer-locked-or-frozen (layerName / layerTbl)
  (setq layerTbl (tblsearch "LAYER" layerName))
  (and layerTbl (/= (logand (cdr (assoc 70 layerTbl)) 5) 0))
)

;; ssgetの文字列フィルタはワイルドカード(wcmatch)として解釈されるため、
;; 特殊文字の前に ` を付けて文字どおりに一致させる
(defun qse-escape-wild (s / out i ch)
  (setq out "" i 1)
  (repeat (strlen s)
    (setq ch (substr s i 1))
    (if (vl-string-search ch "#@.*?~[]-,`")
      (setq out (strcat out "`" ch))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  out
)

;; 複数選択された値からssgetフィルタのサブリストを作る
(defun qse-make-subfilter (dxfCode valList / subLst itemVal)
  (setq subLst '())
  (foreach itemVal valList
    (setq subLst
      (append subLst
        (list
          (cond
            ((= dxfCode 62)
             (if (= itemVal "256 (ByLayer)") '(62 . 256) (cons 62 (atoi itemVal)))
            )
            (T (cons dxfCode (qse-escape-wild itemVal)))
          )
        )
      )
    )
  )
  (if (> (length subLst) 1)
    (append (list '(-4 . "<OR")) subLst (list '(-4 . "OR>")))
    subLst
  )
)

;; 回転角度(度数の文字列リスト)からssgetフィルタを作る。
;; 一覧は小数点2桁に丸めた値なので、完全一致ではなく ±0.005° の範囲で判定する。
;; 0°は360°付近の値も含める。ARCのグループコード50は開始角度なので除外する
(defun qse-angle-range (lo hi)
  (list '(-4 . "<AND") '(-4 . ">=") (cons 50 lo) '(-4 . "<=") (cons 50 hi) '(-4 . "AND>"))
)
(defun qse-make-angle-filter (valList / tol d subLst)
  (setq tol (* 0.005 (/ pi 180.0)) subLst '())
  (foreach v valList
    (setq d (* (atof v) (/ pi 180.0)))
    (setq subLst (append subLst (qse-angle-range (- d tol) (+ d tol))))
    (if (< d tol)
      (setq subLst (append subLst (qse-angle-range (- (* 2.0 pi) tol) (* 2.0 pi))))
    )
  )
  (append (list '(-4 . "<NOT") '(0 . "ARC") '(-4 . "NOT>") '(-4 . "<OR"))
          subLst
          (list '(-4 . "OR>")))
)

;; ブロック参照のブロック名を返す。ダイナミックブロックは "*U123" のような
;; 匿名名になっているので、実効名(EffectiveName)に置き換える
(defun qse-block-name (ent entData / nm obj r)
  (setq nm (cdr (assoc 2 entData)))
  (if (and nm (= (substr nm 1 1) "*"))
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
          (if (and r (not (vl-catch-all-error-p r))) (setq nm r))
        )
      )
    )
  )
  nm
)

;; ブロック参照にぶら下がる属性(ATTRIB)のうち、値がtxtListに含まれるものがあるか
(defun qse-insert-has-att-text (ent txtList / attEnt attData found)
  (setq attEnt (entnext ent))
  (while (and attEnt (not found)
              (/= (cdr (assoc 0 (setq attData (entget attEnt)))) "SEQEND"))
    (if (and (= (cdr (assoc 0 attData)) "ATTRIB")
             (member (cdr (assoc 1 attData)) txtList))
      (setq found T)
    )
    (setq attEnt (entnext attEnt))
  )
  found
)

;; ssgetで絞り込めない条件(ブロック実効名・文字列内容・属性)をLISP側で判定する
;;   selBlks : ブロック参照で、実効名が一覧に含まれること
;;   selTxts : TEXT/MTEXT/ATTDEFは内容が一致、ブロック参照は属性値が一致すること
;;   attOnly : タイプ「属性」が選ばれ「ブロック参照」は選ばれていないとき、
;;             ブロック参照は属性付きのものに限る
(defun qse-post-match (ent entData selBlks selTxts attOnly / typ ok)
  (setq typ (cdr (assoc 0 entData)) ok T)
  (if (and attOnly (= typ "INSERT") (/= (cdr (assoc 66 entData)) 1))
    (setq ok nil)
  )
  (if (and ok selBlks)
    (setq ok (and (= typ "INSERT") (member (qse-block-name ent entData) selBlks)))
  )
  (if (and ok selTxts)
    (setq ok
      (cond
        ((member typ '("TEXT" "MTEXT" "ATTDEF")) (member (cdr (assoc 1 entData)) selTxts))
        ((and (= typ "INSERT") (= (cdr (assoc 66 entData)) 1))
         (qse-insert-has-att-text ent selTxts))
        (T nil)
      )
    )
  )
  ok
)

;; ---------- 閉じたポリライン内選択 用ヘルパー (v7で追加) ----------
;; LWPOLYLINE実体の頂点リストを、現在のUCSの座標として取り出す。
;; ・LWPOLYLINEの頂点(グループコード10)はエンティティのOCS(押し出し方向=210)
;;   上の2D座標＋標高(38)で保持されているため、trans関数でOCS→現在UCSへ変換する。
;; ・円弧セグメント(バルジ)は直線近似(頂点同士を直線で結ぶ)とする。
;;   ほとんどの用途では実用上問題ないが、大きな円弧を含む場合は
;;   境界形状が実際のポリラインよりやや粗くなる点に注意。
(defun qse-poly-points (ent / entData normal elev pts pair)
  (setq entData (entget ent))
  (setq normal (cdr (assoc 210 entData)))
  (if (null normal) (setq normal '(0.0 0.0 1.0)))
  (setq elev (cdr (assoc 38 entData)))
  (if (null elev) (setq elev 0.0))
  (setq pts '())
  (foreach pair entData
    (if (= (car pair) 10)
      (setq pts (cons (trans (list (cadr pair) (caddr pair) elev) normal 1) pts))
    )
  )
  (reverse pts)
)

;; entselで閉じたLWPOLYLINEを1つ選ばせ、頂点リストを返す。
;; 選択が無効(未選択・タイプ違い・未閉合)な場合はnilを返す。
(defun qse-pick-closed-polyline ( / sel ent entData)
  (setq sel (entsel "\n境界とする閉じたポリラインを選択してください: "))
  (cond
    ((null sel)
     (princ "\nオブジェクトが選択されませんでした。")
     nil
    )
    (T
     (setq ent (car sel))
     (setq entData (entget ent))
     (cond
       ((/= (cdr (assoc 0 entData)) "LWPOLYLINE")
        (princ "\nLWPOLYLINE（軽量ポリライン）以外は指定できません。")
        nil
       )
       ((/= (logand (cdr (assoc 70 entData)) 1) 1)
        (princ "\n選択したポリラインは閉じていません（閉合されたポリラインを選択してください）。")
        nil
       )
       (T (qse-poly-points ent))
     )
    )
  )
)

;; ---------- 検索範囲の指定(v13: UIのラジオボタンを廃止しコマンドラインで指定) ----------
;; 「適用」後のプロンプトで次のいずれかを行う。
;;   ・Enter           : AutoCAD標準の「オブジェクトを選択:」(窓=青/交差=緑の標準表示。
;;                       クリック・窓・交差などを繰り返し、Enterで確定)
;;                       何も選ばずにEnterなら図面全体(現在の空間)から検索
;;   ・P               : 閉じたポリラインを選び、その内側を選択
;; 窓選択などの後にポリライン内側選択を追加する機能はない(選択したら確定)。
;; ※ 標準の選択プロンプト内では P は「直前の選択」の意味になるため、
;;    P は標準の選択に入る前のプロンプトで受け付ける。
(defun qse-pick-scope-ss (filterList / k ssRes polyPts)
  (initget "P")
  (setq k
    (getkword
      "\n検索範囲 [P=閉じたポリラインの内側] <Enter=窓選択・クリック（何も選ばなければ図面全体）>: "
    )
  )
  (cond
    ;; P: 閉じたポリラインの内側
    ((= k "P")
     (setq polyPts (qse-pick-closed-polyline))
     (if polyPts
       (setq ssRes
         (if filterList (ssget "WP" polyPts filterList) (ssget "WP" polyPts))
       )
     )
    )
    ;; Enter: AutoCAD標準の選択(窓/交差/クリックを繰り返し可能)
    (T
     (princ "\n対象を選択してください（窓選択・クリック。何も選ばずEnterで図面全体）: ")
     (setq ssRes (if filterList (ssget filterList) (ssget)))
     (if (null ssRes)
       ;; 何も選ばずEnter: 図面全体(現在の空間のみ。モデル/現在のレイアウト)
       (setq ssRes
         (ssget "X"
           (append filterList
             (list (cons 410 (if (and (= (getvar "TILEMODE") 0) (= (getvar "CVPORT") 1))
                               (getvar "CTAB")
                               "Model")))
           )
         )
       )
     )
    )
  )
  ssRes
)

(defun c:QS ( / ssBase i entData objType layerName colorVal ltypeVal blkName txtContent
                   typeList layerList colorList ltypeList blkList txtList
                   blkCountAlist txtCountAlist blkCountList txtCountList
                   rotVal rotDeg rotKey angleCountAlist angleList angleCountList
                   attEnt attData attTxt
                   inFile outFile ps1File fn result
                   selTypes selLayers selColors selLtypes selBlks selTxts selAngles
                   selResult errMsg
                   filterList ssFilter
                   ent attOnly blkPat *error* )
  (vl-load-com)

  ;; Escなどで中断したときも一時ファイルを残さない
  (defun *error* (msg)
    (foreach f (list inFile outFile ps1File)
      (if (and f (findfile f)) (vl-file-delete f))
    )
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nエラー: " msg))
    )
    (princ)
  )

  ;; ---------- 1. 基準オブジェクトの選択 ----------
  (princ "\n基準となるオブジェクトを選択してください（複数選択可、選択後Enter）: ")
  (setq ssBase (ssget))

  (if (null ssBase)
    (progn (princ "\nオブジェクトが選択されませんでした。中止します。") (princ))
    (progn
      (setq typeList '() layerList '() colorList '() ltypeList '() blkList '() txtList '())
      (setq blkCountAlist '() txtCountAlist '() angleCountAlist '())

      (repeat (setq i (sslength ssBase))
        (setq entData (entget (ssname ssBase (setq i (1- i)))))
        (setq layerName (cdr (assoc 8 entData)))

        ;; ロック・凍結されたレイヤーのオブジェクトは、条件設定(各項目欄の
        ;; 一覧)には含めない。基準選択に含まれていても、ここで丸ごと無視する
        (if (not (qse-layer-locked-or-frozen layerName))
          (progn
            (setq objType (cdr (assoc 0 entData)))
            (if (and objType (not (member objType typeList)))
              (setq typeList (cons objType typeList)))

            (if (and layerName (not (member layerName layerList)))
              (setq layerList (cons layerName layerList)))

            (setq colorVal (cdr (assoc 62 entData)))
            (if (null colorVal) (setq colorVal "256 (ByLayer)") (setq colorVal (itoa colorVal)))
            (if (not (member colorVal colorList))
              (setq colorList (cons colorVal colorList)))

            (setq ltypeVal (cdr (assoc 6 entData)))
            (if (null ltypeVal) (setq ltypeVal "ByLayer"))
            (if (not (member ltypeVal ltypeList))
              (setq ltypeList (cons ltypeVal ltypeList)))

            ;; 回転角度(DXFグループコード50、ラジアン)を度数に変換し、
            ;; 0〜360度の範囲に正規化して小数点2桁に丸めたうえで件数集計する。
            ;; 回転の概念がないオブジェクト(LINE/CIRCLEなど)は単に無視される。
            ;; ARCのグループコード50は「開始角度」で回転角度ではないので除外する
            (setq rotVal (if (/= objType "ARC") (cdr (assoc 50 entData))))
            (if rotVal
              (progn
                (setq rotDeg (* rotVal (/ 180.0 pi)))
                (while (< rotDeg 0.0)    (setq rotDeg (+ rotDeg 360.0)))
                (while (>= rotDeg 360.0) (setq rotDeg (- rotDeg 360.0)))
                (setq rotKey (rtos rotDeg 2 2))
                ;; 359.996° などは丸めると "360.00" になるので 0° として扱う
                (if (>= (atof rotKey) 360.0) (setq rotKey (rtos 0.0 2 2)))
                (setq angleCountAlist (qse-count-inc angleCountAlist rotKey))
              )
            )

            ;; ブロック参照: ブロック名を件数付きで集計する。
            ;; さらに、そのブロック参照に属性(ATTRIB)がぶら下がっている場合は
            ;; entnextで辿って属性値も「文字列内容」として集計する
            ;; (これが「オブジェクトの中にある文字・ブロックを選ぶ」機能 v6)
            (if (= objType "INSERT")
              (progn
                (setq blkName (qse-block-name (ssname ssBase i) entData))
                (if blkName
                  (setq blkCountAlist (qse-count-inc blkCountAlist blkName)))

                (if (= (cdr (assoc 66 entData)) 1)
                  (progn
                    (setq attEnt (entnext (ssname ssBase i)))
                    (while (and attEnt
                                (/= (cdr (assoc 0 (entget attEnt))) "SEQEND"))
                      (setq attData (entget attEnt))
                      (if (= (cdr (assoc 0 attData)) "ATTRIB")
                        (progn
                          (if (not (member "ATTRIB" typeList))
                            (setq typeList (cons "ATTRIB" typeList)))
                          (setq attTxt (cdr (assoc 1 attData)))
                          (if attTxt
                            (setq txtCountAlist (qse-count-inc txtCountAlist attTxt)))
                        )
                      )
                      (setq attEnt (entnext attEnt))
                    )
                  )
                )
              )
            )

            ;; 文字列系エンティティ(TEXT/MTEXT/ATTDEF/ATTRIB)は内容ごとに件数を集計
            (if (or (= objType "TEXT") (= objType "MTEXT")
                    (= objType "ATTDEF") (= objType "ATTRIB"))
              (progn
                (setq txtContent (cdr (assoc 1 entData)))
                (if txtContent
                  (setq txtCountAlist (qse-count-inc txtCountAlist txtContent)))
              )
            )
          )
        )
      )

      ;; ---------- 1.5 各リストを並び替え(見やすくする) ----------
      ;; それまでは選択セットの内部順(≒処理順)でほぼ無作為な並びに
      ;; なっていたため、一覧性のため以下の基準でソートする。
      ;;   ・オブジェクトタイプ: 日本語ラベルの文字コード順
      ;;   ・色コード         : 数値としての昇順(ByLayerは256扱いで自然に末尾)
      ;;   ・それ以外         : 文字コード順(昇順)
      ;; ブロック名・文字列内容は件数集計(alist)から一覧を作り直したうえで
      ;; ソートし、ソート後の並びに合わせて件数リストも作る。
      (setq typeList
        (vl-sort typeList '(lambda (a b) (< (qse-type-label a) (qse-type-label b))))
      )
      (setq layerList (vl-sort layerList '<))
      (setq colorList
        (vl-sort colorList '(lambda (a b) (< (atoi a) (atoi b))))
      )
      (setq ltypeList (vl-sort ltypeList '<))

      (setq blkList (vl-sort (mapcar 'car blkCountAlist) '<))
      (setq blkCountList
        (mapcar '(lambda (v) (itoa (cdr (assoc v blkCountAlist)))) blkList)
      )
      (setq txtList (vl-sort (mapcar 'car txtCountAlist) '<))
      (setq txtCountList
        (mapcar '(lambda (v) (itoa (cdr (assoc v txtCountAlist)))) txtList)
      )

      (setq angleList
        (vl-sort (mapcar 'car angleCountAlist) '(lambda (a b) (< (atof a) (atof b))))
      )
      (setq angleCountList
        (mapcar '(lambda (v) (itoa (cdr (assoc v angleCountAlist)))) angleList)
      )

      ;; ---------- 2. 中間ファイルの準備と書き出し ----------
      (setq inFile  (vl-filename-mktemp "qse_in.txt"))
      (setq outFile (vl-filename-mktemp "qse_out.txt"))
      (setq ps1File (vl-filename-mktemp "qse_ui.ps1"))

      (setq fn (open inFile "w"))
      (qse-write-section fn "TYPE"
        (mapcar '(lambda (v) (strcat v "|" (qse-type-label v))) typeList)
      )
      (qse-write-section fn "LAYER" layerList)
      (qse-write-section fn "COLOR" colorList)
      (qse-write-section fn "LTYPE" ltypeList)
      (if blkList (qse-write-section fn "BLOCK" blkList))
      (if blkList (qse-write-section fn "BLOCKCOUNT" blkCountList))
      (if txtList (qse-write-section fn "TEXT"  txtList))
      (if txtList (qse-write-section fn "TEXTCOUNT" txtCountList))
      (if angleList (qse-write-section fn "ANGLE" angleList))
      (if angleList (qse-write-section fn "ANGLECOUNT" angleCountList))
      (close fn)

      ;; 埋め込みPowerShellスクリプトを一時ファイルへ書き出す
      (qse-write-ps1-script ps1File)

      ;; ---------- 3. PowerShell(WPF)ダイアログの起動 ----------
      (princ "\nダイアログを表示中...（別ウィンドウが開きます）")
      (qse-run-ps1 ps1File inFile outFile)

      ;; ---------- 4. 結果ファイルの読み込み ----------
      (if (not (findfile outFile))
        (princ "\n結果ファイルが見つかりません。PowerShellの起動に失敗した可能性があります。")
        (progn
          (setq result (qse-read-output outFile))
          (setq selTypes  (nth 0 result))
          (setq selLayers (nth 1 result))
          (setq selColors (nth 2 result))
          (setq selLtypes (nth 3 result))
          (setq selBlks   (nth 4 result))
          (setq selTxts   (nth 5 result))
          (setq selAngles (nth 6 result))
          (setq selResult (nth 7 result))
          (setq errMsg    (nth 8 result))

          (cond
            ((= selResult "ERROR")
             (princ (strcat "\nPowerShell側でエラーが発生しました: " (if errMsg errMsg "詳細不明")))
            )
            ((/= selResult "OK")
             (princ "\n処理をキャンセルしました。")
            )
            (T
             ;; ---------- 5. ssgetフィルタの構築 ----------
             ;; 属性(ATTRIB)はssgetで直接選べないので、タイプ「属性」は
             ;; ブロック参照に置き換え、属性付きかどうかは後段で判定する
             (setq attOnly (and (member "ATTRIB" selTypes) (not (member "INSERT" selTypes))))
             (if (member "ATTRIB" selTypes)
               (setq selTypes (cons "INSERT" (vl-remove "INSERT" (vl-remove "ATTRIB" selTypes))))
             )

             (setq filterList '())
             (if selTypes  (setq filterList (append filterList (qse-make-subfilter 0 selTypes))))
             (if selLayers (setq filterList (append filterList (qse-make-subfilter 8 selLayers))))
             (if selColors (setq filterList (append filterList (qse-make-subfilter 62 selColors))))
             (if selLtypes (setq filterList (append filterList (qse-make-subfilter 6 selLtypes))))
             ;; ブロック名は実効名で判定するので、ssgetではブロック参照かつ
             ;; 「指定名 または 匿名ブロック(*U...)」まで絞り込み、最終判定は後段で行う
             (if selBlks
               (progn
                 (setq blkPat "`*U*")
                 (foreach v selBlks (setq blkPat (strcat (qse-escape-wild v) "," blkPat)))
                 (setq filterList (append filterList (list '(0 . "INSERT") (cons 2 blkPat))))
               )
             )
             ;; 文字列内容(属性値を含む)はssgetで判定できないので後段で行う
             (if selAngles (setq filterList (append filterList (qse-make-angle-filter selAngles))))

             ;; 検索範囲はコマンドラインで指定する(窓選択 / P=ポリライン内 / Enter=図面全体)
             (setq ssFilter (qse-pick-scope-ss filterList))

             ;; ロック・凍結されたレイヤーのオブジェクトは対象から除外する。
             ;; 窓選択・ポリライン内はAutoCAD標準のピック動作により通常は
             ;; これらが選択候補に入らないが、Enterの「図面全体」(ssget "X")は
             ;; ロック・凍結を無視してヒットするため、ここで統一して除外する。
             ;; あわせて、ssgetで判定できない条件(ブロック実効名・文字列内容・属性)もここで判定する
             (if ssFilter
               (progn
                 (setq i (sslength ssFilter))
                 (while (> i 0)
                   (setq i (1- i))
                   (setq ent (ssname ssFilter i))
                   (setq entData (entget ent))
                   (if (or (qse-layer-locked-or-frozen (cdr (assoc 8 entData)))
                           (not (qse-post-match ent entData selBlks selTxts attOnly)))
                     (setq ssFilter (ssdel ent ssFilter))
                   )
                 )
                 (if (= (sslength ssFilter) 0) (setq ssFilter nil))
               )
             )

             (if ssFilter
               (progn
                 (sssetfirst nil ssFilter)
                 (princ (strcat "\n" (itoa (sslength ssFilter)) " 個のオブジェクトが選択されました。"))
               )
               (princ "\n条件に合致するオブジェクトは見つかりませんでした。")
             )
            )
          )
        )
      )

      ;; ---------- 6. 後片付け ----------
      (if (findfile inFile)  (vl-file-delete inFile))
      (if (findfile outFile) (vl-file-delete outFile))
      (if (findfile ps1File) (vl-file-delete ps1File))
    )
  )
  (princ)
)
(princ "\n[QS] 読み込まれました。'QS' で実行できます。(単一ファイル/PowerShell-WPF UI版・v13:検索範囲をコマンドライン指定に変更(標準の窓選択・クリック/P=ポリライン内/選択なしEnter=図面全体)・v12:ブロック名の検索欄を追加・v11:ワイルドカード文字/ダイナミックブロック/属性値の選択を修正・v10:ロック凍結レイヤー除外・[7]回転角度を追加・フルスクリーン展開マウスホイール対応・フッター/ヘッダー領域縮小・右クリックで決定・UI拡大・文字サイズ12・検索範囲は前回値・単一項目表示修正)")
(princ)