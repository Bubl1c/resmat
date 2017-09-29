package edu.knuca.resmat.articles

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.http.NotFoundException
import edu.knuca.resmat.utils.SqlUtils
import io.circe.Json
import io.circe.parser.parse

import scala.concurrent.ExecutionContext

case class ArticleDto(id: Long, header: String, preview: String, body: String, visible: Boolean, meta: Json)

class ArticleService(val db: DatabaseService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {
  import edu.knuca.resmat.articles.{ArticleQueries => Q}
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import edu.knuca.resmat.http.JsonProtocol._

  def create(article: ArticleDto): ArticleDto = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createArticle(article).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $article")
    )
    getById(insertedId)
  }

  def update(id: Long, article: ArticleDto): ArticleDto = db.run { implicit c =>
    val affectedRows = Q.updateArticle(id, article).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update $article. No rows affected.")
    }
    getById(id)
  }

  def get(): Seq[ArticleDto] = db.run{ implicit c =>
    Q.getArticles.as(Q.parser.*)
  }

  def getVisible(): Seq[ArticleDto] = db.run{ implicit c =>
    Q.getArticles.as(Q.parser.*).filter(_.visible)
  }

  def getById(id: Long, onlyIfVisible: Boolean = false): ArticleDto = db.run{ implicit c =>
    Q.getArticle(id).as(Q.parser.singleOpt).filter(ad => if(onlyIfVisible) ad.visible else true).getOrElse(
      throw NotFoundException(s"Article with id $id not found!")
    )
  }
}

object ArticleQueries {
  import anorm.SqlParser.{str, int, long, bool}

  object A {
    val table = "articles"
    val id = "id"
    val header = "header"
    val preview = "preview"
    val body = "body"
    val visible = "visible"
    val meta = "meta"
  }

  val parser  = for {
    id <- long(A.id)
    header <- str(A.header)
    preview <- str(A.preview)
    body <- str(A.body)
    visible <- bool(A.visible)
    meta <- str(A.meta)
  } yield ArticleDto(id, header, preview, body, visible, parseMeta(meta))

  def createArticle(article: ArticleDto) =
    SQL(
      s"""INSERT INTO ${A.table} (
         |${A.header},
         |${A.preview},
         |${A.body},
         |${A.visible},
         |${A.meta}
         |) VALUES (
         |{header},
         |{preview},
         |{body},
         |{visible},
         |{meta}
         |)""".stripMargin)
      .on("header" -> article.header)
      .on("preview" -> article.preview)
      .on("body" -> article.body)
      .on("visible" -> article.visible)
      .on("meta" -> article.meta.toString())

  def updateArticle(id: Long, article: ArticleDto) =
    SQL(
      s"""UPDATE ${A.table} SET
         |${A.header}={header},
         |${A.preview}={preview},
         |${A.body}={body},
         |${A.visible}={visible},
         |${A.meta}={meta}
         |WHERE id = {id}""".stripMargin)
      .on("id" -> id)
      .on("header" -> article.header)
      .on("preview" -> article.preview)
      .on("body" -> article.body)
      .on("visible" -> article.visible)
      .on("meta" -> article.meta.toString())

  def getArticles = SqlUtils.get(A.table)

  def getArticle(id: Long) = SqlUtils.get(A.table, id)

  def parseMeta(metaJson: String): Json = parse(metaJson).fold( e =>
    throw new RuntimeException(s"Failed to parse article meta in json: ${metaJson}", e),
    r => r
  )
}