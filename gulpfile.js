var gulp  = require('gulp'),
    shell = require('gulp-shell'),
    watch = require('gulp-watch'),
    batch = require('gulp-batch'),
    debug = require('gulp-debug'),
    webserver = require('gulp-webserver');

gulp.task('watch', function () {
  watch('**/*.elm', batch(function (events, done) {
    gulp.start('build', done);
  }));
});

gulp.task('build', function () {
  return gulp.src('src/Main.elm', { read: false })
    .pipe(shell([
      'echo Building <%= file.path %>',
      'elm-make <%= file.path %> --output elm.js'
    ], {}));
});

gulp.task('webserver', function() {
  gulp.src('.')
    .pipe(webserver({
      livereload: false,
      directoryListing: true//,
      // open: true
    }));
});

gulp.task('default', ['build', 'webserver', 'watch']);
