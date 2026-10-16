(defun c:CF ( / ss p0 ref elast res cnt )
  (vl-load-com)
  (princ "\n=== プレビュー付き 仮起点(FROM)連続複写 ===")
  (princ "\n複写するオブジェクトを選択:")

  ;; 1. オブジェクトの選択
  (setq ss (ssget))

  (if ss
    (if (setq p0 (getpoint "\n基準点を指定: "))
      (progn
        (princ "\n【操作】 仮起点(壁など)をクリック ➔ 方向を向けて距離 or @X,Y … 繰り返し / Enterで終了")
        (princ "\n  ※仮起点を間違えたら → オフセット入力中に Esc で仮起点からやり直し / 複写後なら U で1回戻る")
        (setq cnt 0)

        ;; 2. 仮起点を指定するたびに COPY を1回ずつ実行する
        ;;    （COPYの単一/連続モードに関係なく、Enterまで終わらない）
        (while (progn
                 (initget "Undo")
                 (setq ref (getpoint "\n仮起点を指定 [戻る(U)] <終了>: "))
               )
          (if (= ref "Undo")
            ;; --- 直前の複写を取り消す ---
            (if (> cnt 0)
              (progn
                (command "._UNDO" "1")
                (setq cnt (1- cnt))
                (princ "\n直前の複写を取り消しました。")
              )
              (princ "\nこれ以上は戻れません。")
            )
            ;; --- 複写 ---
            (progn
              (setq elast (entlast))
              ;; 基準点・仮起点は自動入力、オフセットだけ pause でプレビューしながら入力
              ;; Esc で中断されても CF は終わらず、仮起点の指定に戻る
              (setq res
                (vl-catch-all-apply
                  '(lambda ()
                     (command "_.copy" ss "" "_non" p0 "_from" "_non" ref pause)
                     ;; 連続複写モードで COPY が続いている場合は閉じる
                     (if (= (getvar "cmdactive") 1) (command ""))
                   )
                )
              )
              (if (vl-catch-all-error-p res)
                (progn
                  (while (= (getvar "cmdactive") 1) (command))
                  (princ "\n複写を中断しました。仮起点を指定し直してください。")
                )
              )
              ;; 実際に複写できたときだけ戻る回数に数える
              (if (not (equal elast (entlast))) (setq cnt (1+ cnt)))
            )
          )
        )
      )
    )
    (princ "\nオブジェクトが選択されませんでした。")
  )
  (princ)
)

(princ "\n[CF] 仮起点(FROM)連続複写がロードされました。")
(princ)
