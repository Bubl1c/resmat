package edu.knuca.resmat.utils

import java.io.{ByteArrayInputStream, InputStream}

import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.AmazonS3Client

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.model._
import com.typesafe.config.ConfigFactory

import scala.util.Try

object Main extends App {
  val cfg = ConfigFactory.load("aws").getConfig("s3")
  val accessKey = cfg.getString("accessKey")
  val secretKey = cfg.getString("secretKey")
  val bucket = cfg.getString("bucket")
  val manager = new S3Manager(accessKey, secretKey, bucket)

  val testFileName = "testing/test-file.txt"
  val contentBytes = "test file content".getBytes("UTF-8")
  manager.put(testFileName, new ByteArrayInputStream(contentBytes), contentBytes.length).foreach{ result =>
    println("Uploaded file size: " + result.getMetadata.toString)
  }

  manager.get(testFileName).map{ img =>
    println("Downloaded file size: " + img.getObjectMetadata.getContentLength)
  }
}

class S3Manager(accessKey: String, secretKey: String, bucket: String) {
  val s3 = new AmazonS3Client(new BasicAWSCredentials(accessKey, secretKey))
  s3.setRegion(Region.getRegion(Regions.EU_CENTRAL_1))

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
   * Upload an object to your bucket - You can easily upload a file to
   * S3, or upload directly an InputStream if you know the length of
   * the data in the stream. You can also specify your own metadata
   * when uploading to S3, which allows you set a variety of options
   * like content-type and content-encoding, plus additional metadata
   * specific to your applications.
   */
  def put(key: String, inputStream: InputStream, sizeBytes: Long): Try[PutObjectResult] = Try {
    val metadata = new ObjectMetadata()
    metadata.setContentLength(sizeBytes)
    s3.putObject(new PutObjectRequest(bucket, key, inputStream, metadata))
  }

  /**
   * Delete an object - Unless versioning has been turned on for your bucket,
   * there is no way to undelete an object, so use caution when deleting objects.
   */
  def delete(key: String): Try[Unit] = Try{ s3.deleteObject(bucket, key) }

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