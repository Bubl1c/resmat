package edu.knuca.resmat.utils

import java.io.InputStream
import java.util.{UUID}


import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.model._
import com.typesafe.config.ConfigFactory

import scala.util.{Failure, Success, Try}

object Main extends App {
  val cfg = ConfigFactory.load("aws").getConfig("s3")
  val accessKey = cfg.getString("accessKey")
  val secretKey = cfg.getString("secretKey")
  val bucket = cfg.getString("bucket")
  val s3 = new S3Manager(accessKey, secretKey, bucket, "")

  val fileName = "temporary-uploads/4/3cc73b1a-c3db-4852-8eda-9c994bb2d8d7---original-key.Screen Shot 2017-08-14 at 8.24.48 AM.png"
  s3.copyTempObject(fileName, "users/4/tests/1/") match {
    case Success(r) => print("success")
    case Failure(e) => e.printStackTrace()
  }
  //tests here
}

object S3Manager {
  private val tmpFolderName = "temporary-uploads"
  private val tmpKeyDelimiter = "---original-key."
  def userTmpFolder(userId: Long): String = s"$tmpFolderName/$userId/"
  def makeTmpFileName(originalFileName: String): String = UUID.randomUUID() + tmpKeyDelimiter + originalFileName
  def isTmpKey(key: String): Boolean = key.contains(tmpFolderName) && key.contains(tmpKeyDelimiter)

  def originalFromTmpKey(tmpKey: String): String = {
    if(!isTmpKey(tmpKey)) {
      throw new IllegalArgumentException(
        s"Failed to extract the original key. Cannot find delimiter: $tmpKeyDelimiter in S3 temporary key: $tmpKey"
      )
    }
    val delimiterIndex = tmpKey.lastIndexOf(tmpKeyDelimiter)
    tmpKey.substring(delimiterIndex + tmpKeyDelimiter.length)
  }
}

class S3Manager(accessKey: String, secretKey: String, bucket: String, _baseUrl: String) {
  val s3 = new AmazonS3Client(new BasicAWSCredentials(accessKey, secretKey))
  s3.setRegion(Region.getRegion(Regions.EU_CENTRAL_1))

  val baseUrl = _baseUrl + s"/$bucket/"

  def urlFromKey(key: String) = baseUrl + key

  def urlToS3Key(url: String, withFolder: String = ""): String = {
    if(S3Manager.isTmpKey(url)) {
      if(withFolder == "") {
        throw new IllegalArgumentException(s"withFolder shouldn't be empty for temporary keys. url=$url withFolder=$withFolder")
      }
      copyTempObject(keyFromUrl(url), withFolder) match {
        case Success(result) => result
        case Failure(t) => throw new RuntimeException(s"Failed to copy temporary file with url: $url to folder $withFolder. Reason: ", t)
      }
    } else {
      keyFromUrl(url)
    }
  }

  def keyFromUrl(url: String): String = {
    if(url.indexOf(baseUrl) != 0) {
      throw new IllegalArgumentException(s"Cannot retrieve S3 key from URL '$url' as it doesn't contain base url: '$baseUrl'")
    }
    url.substring(baseUrl.length)
  }

  /**
   * Download an object - When you download an object, you get all of
   * the object's metadata and a stream from which to read the contents.
   * It's important to read the contents of the stream as quickly as
   * possibly since the data is streamed directly from Amazon S3 and your
   * network connection will remain open until you read all the data or
   * close the input stream.
   *
   * GetObjectRequest also supports several other options, including
   * conditional downloading of objects based on modification times,
   * ETags, and selectively downloading a range of an object.
   */
  def get(key: String): Try[S3Object] = Try { s3.getObject(new GetObjectRequest(bucket, key)) }

  /**
    * List objects in your bucket by prefix - There are many options for
    * listing the objects in your bucket.  Keep in mind that buckets with
    * many objects might truncate their results when listing their objects,
    * so be sure to check if the returned object listing is truncated, and
    * use the AmazonS3.listNextBatchOfObjects(...) operation to retrieve
    * additional results.
    */
  def list(prefix: String = ""): Try[List[S3ObjectSummary]] = Try {
    import scala.collection.JavaConversions._
    val result = s3.listObjects(new ListObjectsRequest().withBucketName(bucket).withPrefix(prefix))
    result.getObjectSummaries.toList
  }

  /**
   * Upload an object to your bucket - You can easily upload a file to
   * S3, or upload directly an InputStream if you know the length of
   * the data in the stream. You can also specify your own metadata
   * when uploading to S3, which allows you set a variety of options
   * like content-type and content-encoding, plus additional metadata
   * specific to your applications.
   */
  def put(key: String, inputStream: InputStream, sizeBytes: Long): Try[String] = Try {
    val metadata = new ObjectMetadata()
    metadata.setContentLength(sizeBytes)
    s3.putObject(new PutObjectRequest(bucket, PathUtils.normalisePath(key), inputStream, metadata))
    key
  }

  def copyTempObject(tmpKey: String, targetFolder: String): Try[String] = Try {
    val originalKey = S3Manager.originalFromTmpKey(tmpKey)
    val destinationKey = PathUtils.normalisePath(targetFolder, "", "/") + originalKey
    s3.copyObject(bucket, tmpKey, bucket, destinationKey)
    s3.deleteObject(bucket, tmpKey)
    destinationKey
  }

  /**
   * Delete an object - Unless versioning has been turned on for your bucket,
   * there is no way to undelete an object, so use caution when deleting objects.
   */
  def delete(key: String): Try[Unit] = Try{ s3.deleteObject(bucket, key) }

  def deleteFolder(folderPrefix: String): Try[Unit] = Try {
    import scala.collection.JavaConverters._
    list(folderPrefix) match {
      case Success(files) =>
        if(files.nonEmpty) {
          val delReq = new DeleteObjectsRequest(bucket)
          delReq.withKeys(files.map(f => new DeleteObjectsRequest.KeyVersion(f.getKey)).asJava)
          s3.deleteObjects(delReq)
        }
      case Failure(e) => throw e;
    }
  }

  //  } catch {
  //    case ase: AmazonServiceException =>
  //      System.out.println("Caught an AmazonServiceException, which means your request made it " + "to Amazon S3, but was rejected with an error response for some reason.")
  //      System.out.println("Error Message:    " + ase.getMessage)
  //      System.out.println("HTTP Status Code: " + ase.getStatusCode)
  //      System.out.println("AWS Error Code:   " + ase.getErrorCode)
  //      System.out.println("Error Type:       " + ase.getErrorType)
  //      System.out.println("Request ID:       " + ase.getRequestId)
  //    case ace: AmazonClientException =>
  //      System.out.println("Caught an AmazonClientException, which means the client encountered " + "a serious internal problem while trying to communicate with S3, " + "such as not being able to access the network.")
  //      System.out.println("Error Message: " + ace.getMessage)
  //  }
}