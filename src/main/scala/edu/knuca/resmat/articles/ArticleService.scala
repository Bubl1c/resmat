package edu.knuca.resmat.articles

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.http.NotFoundException
import edu.knuca.resmat.tests.TestConfsService
import edu.knuca.resmat.utils.SqlUtils

import scala.concurrent.ExecutionContext

case class ArticleDto(id: Long, header: String, preview: String, body: String, meta: String)
case class ArticleMeta(visible: Boolean)

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

  def getById(id: Long, onlyIfVisible: Boolean = false): ArticleDto = db.run{ implicit c =>
    Q.getArticle(id).as(Q.parser.singleOpt).filter(a => if(onlyIfVisible) decodeMeta(a.meta).visible else true).getOrElse(
      throw new NotFoundException(s"Article with id $id not found!")
    )
  }

  private def decodeMeta(metaJson: String): ArticleMeta = decode[ArticleMeta](metaJson).fold( e =>
    throw new RuntimeException(s"Failed to decode article meta in json: ${metaJson}", e),
    r => r
  )
}

object ArticleQueries {
  import anorm.SqlParser.{str, int, long}

  object A {
    val table = "article"
    val id = "id"
    val header = "header"
    val preview = "preview"
    val body = "body"
    val meta = "meta"
  }

  val parser  = for {
    id <- long(A.id)
    header <- str(A.header)
    preview <- str(A.preview)
    body <- str(A.body)
    meta <- str(A.meta)
  } yield ArticleDto(id, header, preview, body, meta)

  def createArticle(article: ArticleDto) =
    SQL(
      s"""INSERT INTO ${A.table} (
         |${A.header}
         |${A.preview}
         |${A.body}
         |${A.meta}
         |) VALUES (
         |{header},
         |{preview},
         |{body},
         |{meta}
         |)""".stripMargin)
      .on("header" -> article.header)
      .on("preview" -> article.preview)
      .on("body" -> article.body)
      .on("meta" -> article.meta)

  def updateArticle(id: Long, article: ArticleDto) =
    SQL(
      s"""UPDATE ${A.table} SET
         |${A.header}={header},
         |${A.preview}={preview},
         |${A.body}={body},
         |${A.meta}={meta}
         |WHERE id = {id}""".stripMargin)
      .on("id" -> id)
      .on("header" -> article.header)
      .on("preview" -> article.preview)
      .on("body" -> article.body)
      .on("meta" -> article.meta)

  def getArticles = SqlUtils.get(A.table)

  def getArticle(id: Long) = SqlUtils.get(A.table, id)
}