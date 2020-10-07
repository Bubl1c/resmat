provider "aws" {
  region  = "eu-central-1"
  version = "~> 2.0"
  profile = "personal-terraform-admin"
}

terraform {
  required_version = "0.12.26"

  backend "s3" {
    bucket         = "resmat-terraform"
    region         = "eu-central-1"
    profile        = "personal-terraform-admin"
    dynamodb_table = "resmat-terraform"
  }
}