import functions._
import debugs._

class Collect(own: PlayerState, strategy: Array[String]){
  /*  strategy処理  */
  // state true ならCollect状態
  val state = new Array[Boolean](2)
  for(i <- 0 to 1)
    if(strategy(i) == "Collect")
      state(i) = true

  // constants
  val h = 17; val w = 14

  // dp array
  val dpnum_my = Array.ofDim[Int](h, w)
  resetDpNumChizu(dpnum_my)

  // 目的地の状態
  val movelists: Array[List[Char]] = Array(Nil, Nil)  // 移動予定経路リスト
  val goals: Array[Option[(Int, Int)]] = Array(None, None)  // 目標地点



  /* functions */

  // id番の忍者の目的地と経路をデリート
  private def deleteGoal(id: Int): Unit = {
    movelists(id) = Nil
    goals(id) = None
  }

  // goalに指定されてる忍者ソウルを地図から削除
  def delGoalSoul(): Unit = {
    for(i <- 0 to 1 if state(i)){
      val g_xy = goals(i)
      g_xy match{
        case Some(p) =>
          own.allchizu(p._2)(p._1) match{
            case 'S' =>  // 忍者ソウルに特になにも乗ってない
              own.allchizu(p._2)(p._1) = '_'
            case _ =>   // 重複してる
              this.deleteGoal(i)
          }
        case None => ()
      }
    }
  }

  // 現在のゴールが安全(犬来ない)かどうか
  def isGoalSafety(): Unit = {
    for(i <- 0 to 1 if state(i)){
      val g_xy = goals(i)
      g_xy match{
        case Some(p) =>
          (isDogWillCome(p._1, p._2, own.allchizu)) match{
            case true  => // danger
              deleteGoal(i)
            case false => ()
          }
        case None => ()
      }
    }
  }

  // 予定移動先が今も安全かどうか(だめなら経路リセット)
  def isNowMoveSafe(): Unit = {
    for(i <- 0 to 1 if state(i) if state(i)){
      val mlist = movelists(i)
      mlist match{
        // ゴールの安全は保証されているので操作要らない
        case Nil | _::Nil | _::_::Nil => ()
        // このターンでゴールに着かない時
        case a :: b :: l =>
          val xy: (Int,Int) = listMoving(own.ninjas(i), List(a, b))
          isDogWillCome(xy._1, xy._2, own.allchizu) match{
            case true  => deleteGoal(i)
            case false => ()
          }
      }
    }
  }

  // movelistの状態に応じて操作(goalの安全は保証済み)
  // ansを埋めに行く
  def treatMovelist(): Unit = {
    // process function
    def func(i: Int): Unit = {
      val mlist = movelists(i)

      mlist match{
        case Nil =>
          resetDpNumChizu(dpnum_my)
          val gxy: Option[(Int, Int)] = // goalの座標(Noneの可能性あり)
              bfs_soul(own.ninjas(i), (c: Char) => c == 'S', 
                                               own.allchizu, dpnum_my)

          gxy match{
            case Some(p) =>
              own.allchizu(p._2)(p._1) = '_'
              movelists(i) = extractRootFromDpNum(p, dpnum_my)
              func(i) // 改めてoutput準備
            case None => strategy(i) = "Escape"
          }
        case x :: Nil =>
          own.ans(i) += x

          // list作成後一個だけ追加
          resetDpNumChizu(dpnum_my)
          val gxy: Option[(Int, Int)] = // goalの座標(Noneの可能性あり)
              bfs_soul(own.ninjas(i), (c: Char) => c == 'S', 
                                        own.allchizu, dpnum_my)

          gxy match{
            case Some(p) =>
              own.allchizu(p._2)(p._1) = '_'
              movelists(i) = extractRootFromDpNum(p, dpnum_my)
              // 先頭要素だけ取り出し
              own.ans(i) += movelists(i).head
              movelists(i) = movelists(i).tail
            case None => strategy(i) = "Escape"
          }
        case x :: y :: Nil =>
          own.ans(i) += x; own.ans(i) += y
          movelists(i) = Nil
          goals(i) = None
        case x :: y :: l =>
          own.ans(i) += x; own.ans(i) += y
          movelists(i) = l
        case _ => ()  // error
      }
    }

    for(id <- 0 to 1 if state(id))
      func(id)
  }


  /* process */
  def process(): Unit = {
    this.delGoalSoul()
    this.isGoalSafety()
    this.isNowMoveSafe()
    this.treatMovelist()
  }
}
