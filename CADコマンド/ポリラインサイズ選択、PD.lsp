;; ポリラインのバウンディングボックスから (幅 高さ) を返す。失敗時は nil
(defun PD:getsize ( ent / obj minpt maxpt res )
  (setq obj (vlax-ename->vla-object ent))
  (setq res (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt)))
  (if (not (vl-catch-all-error-p res))
    (progn
      (setq minpt (vlax-safearray->list minpt)
            maxpt (vlax-safearray->list maxpt))
      (list (abs (- (car maxpt) (car minpt)))
            (abs (- (cadr maxpt) (cadr minpt))))
    )
  )
)

(defun c:PD ( / wInput hInput modeInput ss i ent obj minpt maxpt pw ph matchSS layName layObj isVisible
               preSS sz sizeList found n )
  (vl-load-com)

  ;; 前回の入力値保持用（初期値設定）
  (if (null *QSP_LAST_W*) (setq *QSP_LAST_W* 1000.0))
  (if (null *QSP_LAST_H*) (setq *QSP_LAST_H* 500.0))

  ;; 選択済み（先行選択）のポリラインがあれば、そのサイズを表示
  (setq preSS (ssget "_I" '((0 . "LWPOLYLINE,POLYLINE"))))
  (if preSS
    (progn
      (setq sizeList nil
            i 0)
      (repeat (sslength preSS)
        (setq sz (PD:getsize (ssname preSS i)))
        (setq i (1+ i))
        (if sz
          (if (setq found
                     (vl-some
                       '(lambda (x)
                          (if (and (equal (car x) (car sz) 1e-4)
                                   (equal (cadr x) (cadr sz) 1e-4))
                            x))
                       sizeList))
            ;; 同サイズが既にあれば個数を加算
            (setq sizeList
                   (subst (list (car found) (cadr found) (1+ (caddr found)))
                          found sizeList))
            ;; 新しいサイズなら追加
            (setq sizeList (append sizeList (list (list (car sz) (cadr sz) 1))))
          )
        )
      )
      (if sizeList
        (progn
          (princ (strcat "\n選択中のポリライン: " (itoa (sslength preSS)) " 個"))
          (foreach x sizeList
            (princ (strcat "\n  幅(X)=" (rtos (car x) 2 4)
                           "  高さ(Y)=" (rtos (cadr x) 2 4)
                           (if (> (length sizeList) 1)
                             (strcat "  (" (itoa (caddr x)) " 個)")
                             "")))
          )
          ;; 先頭のサイズを検索サイズの初期値にする（Enterでそのまま検索）
          (setq *QSP_LAST_W* (car  (car sizeList))
                *QSP_LAST_H* (cadr (car sizeList)))
        )
      )
    )
  )

  ;; 幅 (X) の入力
  (setq wInput (getreal (strcat "\n検索する幅 (X方向) を入力 <" (rtos *QSP_LAST_W* 2 4) ">: ")))
  (if wInput (setq *QSP_LAST_W* wInput))

  ;; 高さ (Y) の入力
  (setq hInput (getreal (strcat "\n検索する高さ (Y方向) を入力 <" (rtos *QSP_LAST_H* 2 4) ">: ")))
  (if hInput (setq *QSP_LAST_H* hInput))

  ;; 選択モードの指定 (標準: 図面全体 / W: 窓選択)
  (initget "W w")
  (setq modeInput (getkword "\n検索範囲を選択 [窓選択(W)] <図面全体>: "))

  ;; 図形取得処理
  (cond
    ((or (= modeInput "W") (= modeInput "w"))
     (princ "\n検索対象のポリラインを窓選択してください:")
     (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE")))))
    (t
     (setq ss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE")))))
  )

  ;; サイズ・画層状態判定・一括選択
  (if ss
    (progn
      (setq matchSS (ssadd))
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        
        ;; 画層の状態チェック (非表示/フリーズ/ロックの判定)
        (setq layName (vla-get-Layer obj))
        (setq layObj (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layName))
        
        ;; 可視状態（フリーズされておらず、かつ非表示でない）かつ非ロックか確認
        (setq isVisible (and (= (vla-get-Freeze layObj) :vlax-false)
                             (= (vla-get-LayerOn layObj) :vlax-true)
                             (= (vla-get-Lock layObj) :vlax-false)))

        (if isVisible
          (progn
            ;; バウンディングボックス（最小・最大座標）を取得
            (vla-GetBoundingBox obj 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt)
                  maxpt (vlax-safearray->list maxpt))
            
            ;; 幅(X)と高さ(Y)を算出 (許容誤差 1e-4)
            (setq pw (abs (- (car maxpt) (car minpt))))
            (setq ph (abs (- (cadr maxpt) (cadr minpt))))

            ;; 完全一致の判定
            (if (and (equal pw *QSP_LAST_W* 1e-4)
                     (equal ph *QSP_LAST_H* 1e-4))
              (ssadd ent matchSS)
            )
          )
        )
      )
      
      (if (> (sslength matchSS) 0)
        (progn
          (sssetfirst nil matchSS)
          (princ (strcat "\n" (itoa (sslength matchSS)) " 個のポリラインを選択しました。"))
        )
        (princ "\n該当するポリラインは見つかりませんでした。")
      )
    )
    (princ "\n対象オブジェクトが選択されませんでした。")
  )
  (princ)
)