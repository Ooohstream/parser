import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._
import better.files._
import better.files.File._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.JsonDSL.WithBigDecimal

import scala.annotation.tailrec

object Main {

  /* Util functions */

  def zipPairs[A](l : List[A]): List[(A,A)] = l match {
    case a :: b :: rest => (a,b) :: zipPairs(rest)
    case _ => Nil
  }

  def toDollars(initialValue: Option[String]): Option[Long] = initialValue match {
    case Some(value) => value.trim.last match {
      case 'B' => Some((value.replaceAll("[\\$\\|B]", "").trim.toDouble * 1000000000).toLong)
      case 'M' => Some((value.replaceAll("[\\$\\|M]", "").trim.toDouble * 1000000).toLong)
      case 'T' => Some((value.replaceAll("[\\$\\|T]", "").trim.toDouble * 1000000000000L).toLong)
      case _ => None
    }
    case None => None

  }

  def transformEmployees(initialEmployees: Option[String]): Option[Int] = initialEmployees match {
    case Some(employees) => Some(employees.replace(",", "").toInt)
    case None => None
  }

  def filterHeadquarters(countryOption: Option[String], initialHeadquartersOption: Option[String]) : Option[List[String]] = countryOption match {
    case Some(country) => initialHeadquartersOption match {
      case Some(headquarters) => country match {
        case "United States" => Some(zipPairs(headquarters.split(",").toList).map(item => item._1))
        case _ => Some(stringToList(headquarters))
      }
      case None => None
    }
    case None => None
    case None => initialHeadquartersOption match {
      case Some(headquarters) => Some(stringToList(headquarters))
      case None => None
    }
  }

  def stringToList(initialString: String): List[String] = {
    initialString.split(',').map(_.trim).toList
  }

  /* Parse functions */

  def parseName(companyHeader: String): Option[String] = {
    val pattern = """(#\d+\s)?([\-\d\'\p{L}\s&\.]+){1}(\(([\w\s]+)\))?""".r
    pattern.findFirstMatchIn(companyHeader) match {
      case Some(m) => Some(m.group(2))
      case None => None
    }
  }

  def parseTicker(companyHeader: String): Option[String] = {
    val pattern = """(#\d+\s)?([\-\d\'\p{L}\s&\.]+){1}(\(([\w\s]+)\))?""".r
    pattern.findFirstMatchIn(companyHeader) match {
      case Some(m) => m.group(3) match {
        case null => None
        case _ => Some(m.group(4).trim)
      }
    }
  }


  def parseFiskalGraph(graph: Element): Map[String, Option[List[Option[Long]]]] = {
    val revenueOption = graph >?> element("div.profile-graph__content:contains(Revenue)") match {
      case Some(revenue) => Some((revenue >> elements(".bar__item.profile-graph__column")).map(item => toDollars(Some(item >> text))).toList)
      case None => None
    }
    val assetsOption = graph >?> element("div.profile-graph__content:contains(Assets)") match {
      case Some(assets) => Some((assets >> elements(".bar__item.profile-graph__column")).map(item => toDollars(Some(item >> text))).toList)
      case None => None
    }
    val profitsOption = graph >?> element("div.profile-graph__content:contains(Profits)") match {
      case Some(profits) => Some((profits >> elements(".bar__item.profile-graph__column")).map(item => toDollars(Some(item >> text))).toList)
      case None => None
    }

    Map("revenue" -> revenueOption, "assets" -> assetsOption, "profits" -> profitsOption)
  }

  def parseElement(document: Document, selector: String): Option[String] = {
    document >?> element(selector) match {
      case Some(found) => Some(found.parent.get.children.toList.last.text)
      case None => None
    }
  }

  def parseDocument(document: Document, directory: File): Unit = {
    val name = parseName(document >> element("div.profile-heading--desktop") >> text("h1"))
    val file : File = (directory/s"${name.get}.json")
    println(name.get)
    val ticker = parseTicker(document >> element("div.profile-heading--desktop") >> text("h1"))
    val marketCap = parseElement(document, "div.profile-datapoint__data-title:contains(Market Cap),span.profile-row--type.profile-row--valuation-text:contains(Market Cap)")
    val industries = parseElement(document, "span.profile-row--type:contains(Industry),span.profile-public-info-label:contains(Industry),span.profile-row--type:contains(Industries)")
    val founded = parseElement(document, "span.profile-row--type:contains(Founded),span.profile-public-info-label:contains(Founded)")
    val country = parseElement(document, "span.profile-public-info-label:contains(Country/Territory),span.profile-row--type:contains(Country/Territory)")
    val ceo = List(
      "CEO",
      "Chief Executive Officer",
      "Co-CEOs & Co-Presidents",
      "Chairman",
      "Chairman and CEO",
      "President").map(title => parseElement(document, s"span.profile-public-info-label:contains(${title}),span.profile-row--type:contains(${title})") match {
      case Some(chief) => Some(stringToList(chief))
      case None => None
    }).find(element => element.isDefined)
    val employees = parseElement(document, "span.profile-public-info-label:contains(Employees),span.profile-row--type:contains(Employees)")
    val sales = parseElement(document, "span.profile-public-info-label:contains(Sales),span.profile-row--type:contains(Sales)")
    val headquarters = parseElement(document, "span.profile-public-info-label:contains(Headquarters),span.profile-row--type:contains(Headquarters)")


    val fiskalGraph = document >?> element(".profile-graph__charts-wrapper") match {
      case Some(graph) => Some(parseFiskalGraph(graph))
      case None => None
    }

    val json = ("companyName" -> name) ~
      ("ticker" -> ticker) ~
      ("marketCap" -> marketCap) ~
      ("industry" -> industries) ~
      ("founded" -> founded) ~
      ("country" -> country) ~
      ("ceo" -> ceo) ~
      ("employees" -> employees) ~
      ("sales" -> sales) ~
      ("headquarters" -> headquarters) ~
      ("fiskalGraph" -> fiskalGraph)

    file.write(compact(render(json)))
  }

  @tailrec def parseNext(baseUrl: String, link: String, browser: Browser, directory : File): Unit = {
    val document: Document = browser.get(link): Document
    document >?> element("a.profile-nav__next") >> attr("href") match {
      case Some(nextLink) =>
        parseDocument(document, directory)
        parseNext(baseUrl, baseUrl + nextLink, browser, directory)
      case None =>
        Map(
          "Samsung Group" -> "https://www.forbes.com/companies/toyota-motor/?list=global2000/",
          "The Home Depot" -> "https://www.forbes.com/companies/shanghai-pudong-development/?list=global2000/",
          "Merck KGaA, Darmstadt, Germany and its affiliates" -> "https://www.forbes.com/companies/eon/?list=global2000/",
          "Fidelity National Information (FIS) (FIS)" -> "https://www.forbes.com/companies/first-republic-bank/?list=global2000/",
          "Essity" -> "https://www.forbes.com/companies/jardine-matheson/?list=global2000/",
          "Carnival Corporation (CUK)" -> "https://www.forbes.com/companies/cj-corporation/?list=global2000/",
          "NVIDIA (NVDA)" -> "https://www.forbes.com/companies/kbc-group/?list=global2000/",
          "#12 Toyota Motor" -> "https://www.forbes.com/companies/alphabet/?list=global2000/"
        ).get(document >> element("div.profile-heading--desktop") >> text("h1"))
        match {
          case Some(nextLink) =>
            parseDocument(document, directory)
            parseNext(baseUrl, nextLink, browser, directory)
          case None =>
        }
    }

  }

  def main(args: Array[String]): Unit = {
    val directory : File = "./jsonDocuments".toFile.createIfNotExists(true, true)
    val browser = JsoupBrowser()
    parseNext("https://www.forbes.com", "https://www.forbes.com/companies/icbc/?list=global2000", browser, directory)
  }

}
