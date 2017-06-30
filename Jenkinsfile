node {
    def root = tool name: 'Go 1.8.3', type: 'go'
    ws("${JENKINS_HOME}/jobs/${JOB_NAME}/builds/${BUILD_ID}/src/github.com/databrary/databrary") {
        withEnv(["GOROOT=${root}", "GOPATH=${JENKINS_HOME}/jobs/${JOB_NAME}/builds/${BUILD_ID}/", "PATH+GO=${root}/bin"]) {
            env.PATH="${GOPATH}/bin:$PATH"

            stage 'Checkout'

            git url: 'https://github.com/databrary/databrary'

            stage 'preTest'
            sh 'go version'
            sh 'go get -u github.com/golang/dep/...'
            sh 'git checkout go_master'
            sh 'dep ensure'
            sh 'whoami'
            sh 'cd Docker && ./docker_build.sh && docker-compose up -d'

            stage 'Test'
            sh 'go vet'
            sh 'go test -cover'

            stage 'Models Test'
            // until
            // https://go-review.googlesource.com/c/38745
            sh 'go test ./db/models/sqlboiler_models/public/ -c config/databrary_dev.toml'
            sh 'go test ./db/models/sqlboiler_models/audit/ -c config/databrary_dev.toml'

            stage 'Build'
            sh 'go build .'

            stage 'Deploy'
            sh 'ls'
        }
    }
}