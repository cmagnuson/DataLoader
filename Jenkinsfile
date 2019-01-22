pipeline {
    agent any

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }
        stage('Build') {
            steps {
                sh '/usr/local/bin/stack setup' 
		sh '/usr/local/bin/stack build --test --bench --haddock'
                archiveArtifacts artifacts: '.stack-work/install/**/bin/**', fingerprint: true 
            }
        }
    }
}
