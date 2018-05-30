package fpinscala.parsing

import scala.language.{higherKinds, implicitConversions}

/**
  * @author caleb
  */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}

    implicit val tok = (s: String) => token(P.string(s))

    def array: Parser[JSON] = surround("[", "]")(value sep ",") map (vs => JArray(vs.toIndexedSeq)) scope "array"

    def obj: Parser[JSON] = surround("{", "}")(pair sep ",") map (ps => JObject(ps.toMap)) scope "object"

    def lit = scope("literal") {
      attempt("null".as(JNull: JSON)) |
      attempt(escapedQuoted.map(JString)) |
      attempt(double.map(JNumber)) |
      attempt("true".as(JBool(true))) |
      "false".as(JBool(false))
    }

    def value = lit | array | obj
    def key = escapedQuoted
    def pair = (key *< ":") ** value

    whitespace *> (array | obj)
//    (whitespace *> surround("{", "}")(pair)).as(JObject(Map()))
  }
}
