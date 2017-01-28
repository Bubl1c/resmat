package edu.knuca.resmat.auth

import org.joda.time.DateTime

case class TokenEntity(id: Option[Long] = None, userId: Long, token: String, created: DateTime, expires: Option[DateTime] = None)
