pipeline {
  agent any
  stages {
    stage('Build') {
      steps {
        script {
          theImage = docker.build "${env.REGISTRY_IMAGE}:${env.BRANCH_TAG}"
        }

      }
    }
    stage('Push Image') {
      parallel {
        stage('For Branch') {
          when {
            not {
              branch 'master'
            }

          }
          steps {
            script {
              docker.withRegistry( "" ) {
                theImage.push()
              }
            }

          }
        }
        stage('For Master') {
          when {
            branch 'master'
          }
          steps {
            script {
              docker.withRegistry( "" ) {
                theImage.push(env.VERSION_TAG)
                theImage.push("latest")
              }
            }

          }
        }
      }
    }
  }
  environment {
    VERSION_TAG = "1.0.${BUILD_NUMBER}"
    BRANCH_TAG = "$VERSION_TAG-$BRANCH_NAME"
    REGISTRY_IMAGE = 'gitlab.issaccorp.com:5000/issac/vor-environment/biproductions'
  }
}