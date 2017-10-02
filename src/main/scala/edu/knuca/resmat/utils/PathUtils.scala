package edu.knuca.resmat.utils

object PathUtils {

  /**
    * Removes all '/', and spaces from the beginning and end of the path
    * Replaces all '/', and spaces between sections of the path
    * Adds prefix and suffix if supplied
    */
  def normalisePath(path: String,
                    withPrefix: String = "",
                    withSuffix: String = "",
                    skipPrefixAndSuffixIfEmpty: Boolean = true,
                    trimSpaces: Boolean = true): String = {
    if (path == "" && skipPrefixAndSuffixIfEmpty) {
      path
    } else {
      // normalise beginning and end of the path
      var normalised = path.replaceAll("^[/\\s]+|[/\\s]+$", "")
      if(trimSpaces) {
        normalised = normalised.replaceAll("\\s+", "")
      }
      // normalise path between it's sections
      withPrefix + normalised.replaceAll("[/\\s]+", "/") + withSuffix
    }
  }

}
