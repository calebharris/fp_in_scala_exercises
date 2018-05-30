package fpinscala.parsing

import fpinscala.UnitSuite
import fpinscala.parsing.JSON.{JArray, JObject, JString}


/**
  * @author caleb
  */
class JSONParserSuite extends UnitSuite {
  private val Parsers = StateParsers
  private val JSONParser = JSON.jsonParser(Parsers)

  test("JSON parser works with objects") {
    val r = Parsers.run(JSONParser)(""" {"key": "value", "key2": "value2" }""")
    r.left.foreach(err => println(err.report))
    assert(r.exists {
      case JObject(m) =>
        val maybeVals = for {
          v1 <- m.get("key")
          v2 <- m.get("key2")
        } yield (v1, v2) match {
          case (JString(s1), JString(s2)) if s1 == "value" && s2 == "value2" => true
          case _ => false
        }
        maybeVals.getOrElse(false)
      case _ => false
    })
  }

  test("JSON parser works with strings") {
    val r = Parsers.run(JSONParser)(""" ["str1", "str2","str3"]""")
    r.left.foreach(err => println(err.report))
    assert(r.exists {
      case JArray(items) => items match {
        case IndexedSeq(JString("str1"), JString("str2"), JString("str3")) => true
        case _ => false
      }
      case _ => false
    })
  }

}
