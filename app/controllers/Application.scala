package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json
import scala.io.Source

object Application extends Controller {

  def index = Action {
    Ok(views.html.index2())
  }

  def words = Action { request =>
    val params = getParam(request.queryString)
	val word = params("number", "7225247386")
    val table = Map("table" -> x.translate(word).toList.sortWith(_.length < _.length))
    Ok(json.Json.toJson(table).toString)
  }

	def getParam(m: Map[String])(Seq[String], param: String, default: String) =
	  (m.get(param).map(_.head)) match {
		  case None => default
		  case Some(x) => x.trim
	  }
}


object x {
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")


  val words: List[String] = in.getLines().toList.filter(i => i.forall(ch => ch.isLetter))


  val mnemonics = Map(
        '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
        '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")


  val charCode: Map[Char, Char] = mnemonics.flatMap{case (k, v) => v.map(i => i -> k)}


  def wordCode(word: String): String = word.toUpperCase map charCode


  val num2words: Map[String, Seq[String]] = words groupBy wordCode


  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
          split <- 1 to number.length
          (num, rest) = number.splitAt(split)
          word <- num2words.getOrElse(num, Seq(num))
          sentence <- encode(rest)
        } yield word :: sentence}.toSet//.filter(list => list.exists(str => str.exists(ch => ch.isLetter)))
  }


  def translate(number: String) = encode(number).
                      filter(list => list.exists(str => str.exists(ch => ch.isLetter))).
                      map(_.reduce((a, b) => if (a.last.isDigit && b.head.isDigit) a + b else a + " " + b))
}
