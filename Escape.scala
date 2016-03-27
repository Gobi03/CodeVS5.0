import functions._
import debugs._


// escape mode の忍者listを引数に取る
class Escape(own: PlayerState, strategy: Array[String]){
  /*  strategy処理  */
  // 実装の兼ね合いで inmode: List[Int] に処理するid入れる
  private def makeInmode(): List[Int] = {
    val test = "Escape"

    // List作って返す
    def func(cnt: Int, res: List[Int]): List[Int] = {
      cnt match{
        case -1 => res
        case _ =>
          if(strategy(cnt) == test // && own.ans(cnt) == ""
          )
            func(cnt-1, cnt :: res)
          else
            func(cnt-1, res)
      }
    }

    return func(1, Nil)
  }

  // inmode宣言
  val inmode = makeInmode()


  // constants
  val h = 17; val w = 14


  /*  functions  */
  // DW_O の情報だけ書いた地図作成
  def makeDogChizu(): Array[Array[Char]] = {
    val dogchizu = copy_chizu(own.chizu)

    for(xy <- own.dogs)
      dogchizu(xy._2)(xy._1) = 'D'

    return dogchizu
  }

  // 忍犬の進行可能エリアを記入
  def setDogCome(dogCome: Array[Array[Boolean]]): Unit = {
    for(xy <- own.dogs){
      val x = xy._1; val y = xy._2
      dogCome(y)(x) = true

      val tmpl = uldrList(x, y)
      for(p <- tmpl){
        val ne_x = p._1._1; val ne_y = p._1._2
        dogCome(ne_y)(ne_x) = true
      }
    }
  }


  // DW_O の地図
  val dogchizu: Array[Array[Char]] = makeDogChizu()
  // dogが要るor来る場所をtrueに
  val dogCome = Array.ofDim[Boolean](h, w)
  setDogCome(dogCome)


  // 2コマ以内に安全地帯があるか(あるなら移動コマンドを、ないならNoneを返す)
  // できるだけ動かないように実装
  def whereShouldIGo(xy: (Int, Int)): Option[String] = {
    var res: Option[String] = None

    def func(nxy: (Int, Int), cnt: Int, com: String): Unit = {
      val nx = nxy._1; val ny = nxy._2

      cnt match{
        case 0 =>
          dogCome(ny)(nx) || dogchizu(ny)(nx) == 'O' match{
            case true  => ()
            case false => res = Some(com)
          }
        case _ =>
          // next calling
          if(dogchizu(ny)(nx) != 'O'){ // 岩じゃなかったら次呼ぶ
            val nextlist = uldrList(nx, ny)
            for(e <- nextlist)
              func(e._1, cnt-1, com + e._2)
          }

          // now processing
          dogCome(ny)(nx) || dogchizu(ny)(nx) == 'O' match{
            case true  => ()
            case false => res = Some(com)
          }
      }
    }

    func(xy, 2, "")
    return res
  }


  // whereShouldIGo がダメだったときに呼ぶ
  // dogcomeに忍者(N)加筆してるので注意
  // 岩をうごかせないかチェック
  // 実装むずいのでとりあえず一マス動かす実装
  def canIPushRock(xy: (Int, Int)): Option[String] = {
    var res: Option[String] = None

    val nx = xy._1; val ny = xy._2
    val nextlist = uldrList(nx, ny)
    for(e <- nextlist){ // e = nec_xy
      val necnec_xy = functions.moving(e._1, e._2)
      // 次点に犬来ないプラス次次点が床
      if(!dogCome(e._1._2)(e._1._1) &&
              dogchizu(necnec_xy._2)(necnec_xy._1) == '_'){
        res = Some(e._2.toString)
      }
    }

    return res
  }



  /// ------------------------------
      /* こっからescape忍術関数 */
  /// ------------------------------
  // dogCome: Arrray[Array[Bool]]
  // dogChizu: Array[Array[Char]]  WO_D

  // 分身の術
  // def makeAvatarMy(id: Int)



  // ------------------------------
  /*  process  */
  def process(): Unit = {
    inmode match{
      case a :: Nil =>  // Escape mode １人
        val ano = (a + 1) % 2  // 自分じゃない方
        val xy = own.ninjas(a)

        // とりあえず簡単に逃げる
        val escgoal = whereShouldIGo(xy)  // 逃げる目的地
        escgoal match{
          case Some(s) => own.ans(a) = s  // あるならそこへ
          case None =>  // ないなら
            // まずは岩動かせないか見る
            canIPushRock(xy) match{
              case Some(s2) => own.ans(a) = s2
              case None => // 岩動かせないならskill using
                own.use_skill = Some("cutting")
                own.skill_sub(0) = a
            }
        }
      case a :: b :: Nil =>  // Escape mode ２人
        val assasin = List(a, b)
        for(id <- assasin){
          // とりあえず簡単に逃げる
          val xy = own.ninjas(id)

          val tmp = whereShouldIGo(xy)
          tmp match{
            case Some(s) => own.ans(id) = s
            case None =>
              // まずは岩動かせないか見る
              canIPushRock(xy) match{
                case Some(s2) => own.ans(id) = s2
                case None => // 岩動かせないならskill using
                  own.use_skill = Some("cutting")
                  own.skill_sub(0) = id
              }
          }
        }
      case _ => () // error case
    }
  }
}
