import debugs._

/* 関数置き場 */
object functions{
  def isRock(c: Char) = List('O', 'o').contains(c)  // その座標に岩があるか
  def isDog(c: Char) = List('D', 'd').contains(c)  // その座標に忍犬がいるか
  def isSoul(c: Char) = List('S', 'o', 'd').contains(c)  // その座標にソウルがあるか

  // 受け取った座標は正規の範囲(壁も除く)にあるかどうか
  private def isCoordinateCorrect(xy: (Int, Int)): Boolean = {
    val x = xy._1; val y = xy._2
    if(x > 0 && y > 0 && x < 13 && y < 16)
      return true
    else
      return false
  }

  // 座標とコマンド受け取って移動後の座標返す
  def moving(cor: (Int, Int), com: Char): (Int, Int) = {
    val nx = cor._1; val ny = cor._2

    com match{
      case 'U' => (nx, ny-1)
      case 'D' => (nx, ny+1)
      case 'R' => (nx+1, ny)
      case 'L' => (nx-1, ny)
      case  _  => cor  // error or 'N' or ''
    }
  }

  // 座標とコマンドリスト受け取って移動後の座標返す(listは先頭から見ていく)
  def listMoving(cor: (Int, Int), movelist: List[Char]): (Int, Int) = {
    def func(ncor: (Int, Int), mlist: List[Char]): (Int, Int) = {
      mlist match{
        case Nil => cor
        case c :: list => func(this.moving(ncor, c), list)
      }
    }

    return func(cor, movelist)
  }

  // 方向を反転させる
  def revVector(c: Char): Char = {
    c match{
      case 'U' => 'D'
      case 'D' => 'U'
      case 'R' => 'L'
      case 'L' => 'R'
      case _ => 'N'  // error
    }
  }

  // 正規の範囲(壁も除く)で上左下右に動くための座標と方向のタプルリストを返す
  def uldrList(x: Int, y: Int): List[((Int, Int), Char)] = {
    var l: List[((Int, Int), Char)] = Nil
    if(isCoordinateCorrect((x+1, y)))
      l = ((x+1, y), 'R') :: l
    if(isCoordinateCorrect((x, y+1)))
      l = ((x, y+1), 'D') :: l
    if(isCoordinateCorrect((x-1, y)))
      l = ((x-1, y), 'L') :: l
    if(isCoordinateCorrect((x, y-1)))
      l = ((x, y-1), 'U') :: l

    return l
  }

  // 忍者ソウル用BFS(目的地の座標optionを返す) Noneの場合サーチ失敗
  // dp_mapは初期値 -1
  // fが目的地かどうかのチェックの述語
  def bfs_soul(xy: (Int, Int), goal_f: Char => Boolean,
    chizu: Array[Array[Char]],
    dp_map: Array[Array[Int]]): Option[(Int, Int)] =
  {
    var ans: Option[(Int, Int)] = None
    val q = new scala.collection.mutable.Queue[((Int, Int), Int)]  // (座標, cnt)

    // 初期値追加
    val tmp = (xy, 0)  // type check通すためにかます
    q += tmp

    // BFS開始
    while(!q.isEmpty){
      val now = q.dequeue  // (座標, cnt)
      val (nx, ny) = (now._1._1, now._1._2)
      val nowc = chizu(ny)(nx)
      val cnt = now._2

      if(dp_map(ny)(nx) == -1){  // まだ来たことないなら
        dp_map(ny)(nx) = cnt

        if(this.isDogWillCome(nx, ny, chizu) || this.isRock(nowc))  // 移動不可エリア
          dp_map(ny)(nx) = 1000
        else if(goal_f(nowc)){
          ans = Some(nx, ny)
          q.clear  // qを空にする
        }
        else{
          val nextl = uldrList(nx, ny)
          for(e <- nextl){
            val tmp2 = (e._1, cnt+1)  // type check通すためにかます
            q += tmp2
          }
        }
      }
    }

    return ans  // 目的地の座標(x, y)
  }

  // dp_num_chizuから最短経路抽出
  def extractRootFromDpNum(goal: (Int, Int), chizu: Array[Array[Int]]): List[Char] = {
    val (gx, gy) = goal
    val cnt = chizu(gy)(gx)

    var ans: List[Char] = Nil

    def dfs(now: (Int, Int), cnt: Int, res: List[Char]): Unit = {
      val (nx, ny) = now
      val cnt_n = chizu(ny)(nx)

      if(cnt_n == cnt)
        cnt match{
          case 0 => ans = res
          case _ => {
            val veclt = uldrList(nx, ny)
            for(p <- veclt)
              dfs(p._1, cnt-1, this.revVector(p._2) :: res)
          }
        }
    }

    // call dfs
    dfs(goal, cnt, Nil)

    return ans
  }

  // 地図をchar listに変換しつつ複製
  def copy_chizu(map: Array[String]): Array[Array[Char]] = {
    val res = new Array[Array[Char]](17)
    for(i <- 0 until 17)
      res(i) = map(i).toArray
    return res
  }

  // dp地図をすべてfalseに
  def resetDpChizu(chizu: Array[Array[Boolean]]): Unit = {
    for(y <- 0 until 17; x <- 0 until 14)
      if(chizu(y)(x))
        chizu(y)(x) = false
  }

  // dp地図をすべて-1に
  def resetDpNumChizu(chizu: Array[Array[Int]]): Unit = {
    for(y <- 0 until 17; x <- 0 until 14)
      if(chizu(y)(x) != -1)
        chizu(y)(x) = -1
  }

  // 指定の座標にそのターン忍犬が来うるか(trueならそのターンそこにいると捕まる)
  def isDogWillCome(x: Int, y: Int, allchizu: Array[Array[Char]]): Boolean = {
    if(this.isDog(allchizu(y)(x)))
      return true
    else{
      val movelist = this.uldrList(x, y)  // 1マス移動する座標のリスト
      val blist = movelist.map(xy => this.isDog(allchizu(xy._1._2)(xy._1._1)))  // trueあるなら犬に捕まる
      return blist.contains(true)
    }
  }
}
