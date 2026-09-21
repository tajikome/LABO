;; ====================================================================
;; コマンド名: BOO
;; 概要: 境界自動生成 ＆ ポリライン連続頂点削除ツール
;; --------------------------------------------------------------------
;; 【主な機能】
;;  1. 事前選択、既存オブジェクト選択、壁内クリックによる自動境界作図に対応。
;;  2. 自動作成失敗時、隙間（孤立端点）にマーカー（半径100の円）を自動描画。
;;  3. 連続「窓選択」により、不要な頂点を一括削除（Enterで終了）。
;;  4. 旧形式ポリライン（2D/3D）の自動軽量化変換に対応。
;;  5. 頂点削除時もバルジ（円弧データ）や幅情報を完璧に保持。
;;  6. 頂点数の安全ガード（閉じたポリラインは最低3点、開いたポリラインは最低2点）。
;;  7. ユーザー座標系（UCS）対応、およびリアルタイムなグリップ再表示。
;; ====================================================================

(defun c:BOO ( / ss ent entType mode pt p1 p2 p1_ocs p2_ocs minX maxX minY maxY 
                 filteredVertexList loopFlag oldCmdecho oldOsmode oldPedit *error* 
                 lastEnt checkSs i chkEnt obj sp ep gaps markerSize tol 
                 IsTouchingOthers vertexList curVertex vPt FixToLWPoly ShowGrips
                 eg headerData footerData inVertices flag70 isClosed minVertices newHeader newEg )
  
  (vl-load-com)

  ;; --- エラー・中断時の安全な復元処理 ---
  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if oldPedit (setvar "PEDITACCEPT" oldPedit))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*"))
      (princ (strcat "\nエラー: " msg))
    )
    (princ "\n[BOO] コマンドを中断しました。設定を復元しました。")
    (princ)
  )

  ;; --- サブ関数：指定図形に青いグリップ（選択状態）を表示 ---
  (defun ShowGrips (e / tempSs)
    (if (and e (entget e))
      (progn
        (setq tempSs (ssadd e))
        (sssetfirst tempSs tempSs)
      )
    )
  )

  ;; --- サブ関数：旧形式ポリラインや線分をLWPOLYLINE（軽量ポリライン）に変換 ---
  (defun FixToLWPoly (e / eType)
    (if e
      (progn
        (setq eType (cdr (assoc 0 (entget e))))
        (cond
          ((= eType "LWPOLYLINE") e)
          ((= eType "POLYLINE")
           (command "_.convertpoly" "_L" e "")
           (entlast)
          )
          ((member eType '("LINE" "ARC"))
           (command "_.pedit" e "")
           (entlast)
          )
          (t nil)
        )
      )
    )
  )

  ;; --- サブ関数：指定した端点が他の線分に触れているかを判定 ---
  (defun IsTouchingOthers (pt source-ent tol / p1 p2 crossSs j cEnt cObj pt-on-curve touch)
    (setq p1 (list (- (car pt) tol) (- (cadr pt) tol))
          p2 (list (+ (car pt) tol) (+ (cadr pt) tol)))
    (setq crossSs (ssget "C" p1 p2 '((0 . "LINE,LWPOLYLINE,ARC,POLYLINE"))))
    (setq touch nil)
    (if crossSs
      (progn
        (setq j 0)
        (while (and (< j (sslength crossSs)) (not touch))
          (setq cEnt (ssname crossSs j))
          (if (not (eq cEnt source-ent))
            (progn
              (setq cObj (vlax-ename->vla-object cEnt))
              (if (not (vl-catch-all-error-p (setq pt-on-curve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list cObj pt)))))
                (if (and pt-on-curve 
                         (<= (distance (list (car pt) (cadr pt)) (list (car pt-on-curve) (cadr pt-on-curve))) tol))
                  (setq touch T)
                )
              )
            )
          )
          (setq j (1+ j))
        )
      )
    )
    touch
  )

  ;; システム設定の退避・変更
  (setq oldCmdecho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setq oldPedit (getvar "PEDITACCEPT"))
  
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0) 
  (setvar "PEDITACCEPT" 1)
  
  ;; ====================================================
  ;; 1. 対象図形の取得・決定フェーズ
  ;; ====================================================
  (setq ss (ssget "I"))
  
  ;; パターンA: 事前選択されている場合
  (if (and ss (= (sslength ss) 1))
    (progn
      (setq ent (ssname ss 0))
      (setq ent (FixToLWPoly ent))
      (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
        (progn
          (princ "\n[事前選択] 選択されたポリラインを取得しました。")
          (setq loopFlag T)
        )
      )
    )
    ;; パターンB: 事前選択がない場合
    (progn
      (if ss (sssetfirst nil nil))
      (initget "Auto Edit")
      (setq mode (getkword "\n実行するモードを選んでください [自動作図(Auto)/既存編集(Edit)] : "))
      (if (not mode) (setq mode "Auto"))
      
      (cond
        ;; --- 自動作図モード ---
        ((= mode "Auto")
         (setvar "OSMODE" oldOsmode) 
         (setq pt (getpoint "\n壁で囲まれた内側の点をクリックしてください: "))
         (setvar "OSMODE" 0)
         
         (if pt
           (progn
             (setq lastEnt (entlast))
             (princ (strcat "\n[診断] 指定座標: X=" (rtos (car pt) 2 2) ", Y=" (rtos (cadr pt) 2 2) " で境界作成を実行します。"))
             
             (setvar "CMDECHO" 1)
             (command "._-boundary" pt "")
             (while (> (getvar "CMDACTIVE") 0) (command ""))
             (setvar "CMDECHO" 0)
             
             (setq ent (entlast))
             
             (if (and ent (not (eq lastEnt ent)) (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "POLYLINE")))
               (progn
                 (setq ent (FixToLWPoly ent))
                 (princ "\n[成功] ポリラインの自動作成に成功しました。")
                 (setq loopFlag T)
               )
               
               ;; 自動作成エラー時の隙間検出・マーカー作図
               (progn
                 (princ "\n==================================================")
                 (princ "\n[注意] 自動作図に失敗しました。")
                 (princ "\n==================================================")
                 (princ "\n[解析中] 画面内の線分から孤立した端点（隙間）を検索しています...")
                 
                 (setq markerSize 100.0)
                 (setq tol (/ (getvar "VIEWSIZE") 500.0))
                 (if (< tol 0.1) (setq tol 0.1))
                 
                 (setq checkSs (ssget "W" (getvar "VSMIN") (getvar "VSMAX") '((0 . "LINE,LWPOLYLINE,ARC,POLYLINE"))))
                 (setq gaps nil)
                 
                 (if checkSs
                   (progn
                     (setq i 0)
                     (while (< i (sslength checkSs))
                       (setq chkEnt (ssname checkSs i))
                       (setq obj (vlax-ename->vla-object chkEnt))
                       
                       (if (not (and (= (cdr (assoc 0 (entget chkEnt))) "LWPOLYLINE") (eq (vlax-curve-isClosed obj) :vlax-true)))
                         (progn
                           (if (not (vl-catch-all-error-p (setq sp (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))))
                             (if (and sp (not (IsTouchingOthers sp chkEnt tol)))
                               (setq gaps (cons sp gaps))
                             )
                           )
                           (if (not (vl-catch-all-error-p (setq ep (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))))
                             (if (and ep (not (IsTouchingOthers ep chkEnt tol)))
                               (setq gaps (cons ep gaps))
                             )
                           )
                         )
                       )
                       (setq i (1+ i))
                     )
                   )
                 )
                 
                 (if gaps
                   (progn
                     (foreach gapPt gaps
                       (entmake (list '(0 . "CIRCLE") '(100 . "AcDbEntity") '(100 . "AcDbCircle") 
                                      (cons 10 gapPt) (cons 40 markerSize) '(62 . 50)))
                       (princ (strcat "\n  -> 隙間検出: X=" (rtos (car gapPt) 2 2) ", Y=" (rtos (cadr gapPt) 2 2) " に半径100のマーカー（色50）を作図"))
                     )
                     (princ (strcat "\n[完了] 合計 " (itoa (length gaps)) " 箇所の隙間にマーカーを描画しました。"))
                   )
                   (princ "\n[完了] 画面内に明確な孤立端点が見つかりませんでした。")
                 )
                 (setq loopFlag nil)
               )
             )
           )
         )
        )
        
        ;; --- 既存オブジェクト編集モード ---
        ((= mode "Edit")
         (setvar "OSMODE" oldOsmode)
         (setq ent (car (entsel "\n編集するポリラインまたは線分を選択してください: ")))
         (setvar "OSMODE" 0)
         
         (if ent
           (progn
             (setq ent (FixToLWPoly ent))
             (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
               (progn
                 (princ "\n[選択成功] ポリラインを取得しました。")
                 (setq loopFlag T)
               )
               (progn
                 (princ "\n[エラー] 選択されたオブジェクトをポリラインに変換できませんでした。")
                 (setq loopFlag nil)
               )
             )
           )
         )
        )
      )
    )
  )
  
  ;; ====================================================
  ;; 2. 連続窓選択による頂点削除フェーズ
  ;; ====================================================
  (if loopFlag
    (progn
      (ShowGrips ent)
      
      (princ "\n--- 連続窓選択による頂点削除モード ---")
      (princ "\n不要な頂点を窓で囲んでください（連続削除可能。終了はEnter）。")
      
      (while (progn
               (setvar "OSMODE" oldOsmode)
               (initget 128)
               (setq p1 (getpoint "\n[連続削除] 窓の1点目を指定 [終了:Enter]: "))
               (and p1 (= (type p1) 'LIST))
             )
        (setq p2 (getcorner p1 "\n[連続削除] 窓の2点目を指定: "))
        (setvar "OSMODE" 0)
        
        (if p2
          (progn
            ;; 取得座標(UCS)をオブジェクト座標系(OCS)へ転送
            (setq p1_ocs (trans p1 1 ent)
                  p2_ocs (trans p2 1 ent))
            
            (setq minX (min (car p1_ocs) (car p2_ocs))
                  maxX (max (car p1_ocs) (car p2_ocs))
                  minY (min (cadr p1_ocs) (cadr p2_ocs))
                  maxY (max (cadr p1_ocs) (cadr p2_ocs)))
            
            ;; DXFデータの解析（ヘッダー、頂点データ構造、フッターの分離）
            (setq eg (entget ent))
            (setq headerData nil
                  vertexList nil
                  footerData nil
                  curVertex nil
                  inVertices nil)
            
            (foreach item eg
              (cond
                ((= (car item) 10)
                 (setq inVertices T)
                 (if curVertex (setq vertexList (append vertexList (list curVertex))))
                 (setq curVertex (list item))
                )
                ((and inVertices (member (car item) '(40 41 42 91)))
                 (setq curVertex (append curVertex (list item)))
                )
                ((and inVertices (not (member (car item) '(10 40 41 42 91))))
                 (if curVertex
                   (progn
                     (setq vertexList (append vertexList (list curVertex)))
                     (setq curVertex nil)
                   )
                 )
                 (setq inVertices nil)
                 (setq footerData (append footerData (list item)))
                )
                ((not inVertices)
                 (setq headerData (append headerData (list item)))
                )
                (t
                 (setq footerData (append footerData (list item)))
                )
              )
            )
            (if curVertex (setq vertexList (append vertexList (list curVertex))))
            
            ;; 窓の範囲外にある頂点データのみ保持
            (setq filteredVertexList nil)
            (foreach vData vertexList
              (setq vPt (cdr (assoc 10 vData)))
              (if (not (and (>= (car vPt) minX) (<= (car vPt) maxX)
                            (>= (cadr vPt) minY) (<= (cadr vPt) maxY)))
                (setq filteredVertexList (append filteredVertexList (list vData)))
              )
            )
            
            ;; 閉じているかに応じた必須頂点数ガード設定
            (setq flag70 (cdr (assoc 70 eg)))
            (setq isClosed (and flag70 (= (logand flag70 1) 1)))
            (setq minVertices (if isClosed 3 2))
            
            (cond
              ;; 1. 最小必須頂点数を下回る場合はキャンセル
              ((< (length filteredVertexList) minVertices)
               (princ (strcat "\n-> [エラー] 削除後の頂点数が " (itoa (length filteredVertexList)) 
                              " 個になります（" (if isClosed "閉じた" "開いた") "ポリラインは最低 " (itoa minVertices) " 個必要）。削除を中止しました。"))
              )
              
              ;; 2. 正常削除処理 (entmod による安全な書き換え)
              ((< (length filteredVertexList) (length vertexList))
               (setq newHeader nil)
               (foreach item headerData
                 (if (= (car item) 90)
                   (setq newHeader (append newHeader (list (cons 90 (length filteredVertexList)))))
                   (setq newHeader (append newHeader (list item)))
                 )
               )
               
               (setq newEg newHeader)
               (foreach vData filteredVertexList
                 (setq newEg (append newEg vData))
               )
               (setq newEg (append newEg footerData))
               
               (entmod newEg)
               (entupd ent)
               
               (ShowGrips ent)
               (princ (strcat "\n-> 頂点を削除しました (残り: " (itoa (length filteredVertexList)) "個)"))
              )
              
              ;; 3. 該当頂点なし
              (t
               (princ "\n-> 指定した窓内に削除対象の頂点はありませんでした。")
              )
            )
          )
        )
      )
      
      (ShowGrips ent)
      (princ "\nすべての編集を終了しました。ポリラインのグリップを表示しています。")
    )
  )
  
  ;; 設定の復元
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (if oldOsmode (setvar "OSMODE" oldOsmode))
  (if oldPedit (setvar "PEDITACCEPT" oldPedit))
  (princ)
)

;; ====================================================
;; 3. ロード時メッセージ ＆ ヘルプコマンド機能
;; ====================================================

;; ファイルロード時の案内メッセージ
(princ "\n==================================================")
(princ "\n[BOO] 境界作成・連続頂点削除ツール がロードされました。")
(princ "\n  ・実行コマンド: BOO")
(princ "\n  ・ヘルプ表示  : BOO-HELP (ダイアログで使い方を表示)")
(princ "\n==================================================")
(princ)

;; ヘルプダイアログを表示する追加コマンド (BOO-HELP)
(defun c:BOO-HELP ()
  (alert 
    (strcat
      "【BOO コマンド取扱説明書】\n\n"
      "■ 概要\n"
      "自動で境界ポリラインを作成し、不要な頂点を連続して「窓選択」で削除できるツールです。\n\n"
      "■ 操作手順\n"
      "1. コマンド「BOO」を実行します。\n"
      "2. モードを選択します：\n"
      "   ・[Auto] 壁で囲まれた内側の点をクリック（境界自動作成）\n"
      "   ・[Edit] 画面上のポリラインや線分をクリックして選択\n"
      "   ※あらかじめオブジェクトを選択した状態で「BOO」を実行すると直接削除に入ります。\n"
      "3. 削除したい頂点をマウスの「窓選択（2点クリック）」で囲みます。\n"
      "4. 何度でも連続して削除できます。Enterキーで終了します。\n\n"
      "■ 補足・安全機能\n"
      "・境界作図に失敗した場合、隙間（孤立端点）にマーカーを描画します。\n"
      "・頂点を削っても線の円弧（バルジ）や線幅は保持されます。\n"
      "・閉じたポリラインは最低3点、開いたポリラインは最低2点を下回る削除はブロックされます。"
    )
  )
  (princ)
)