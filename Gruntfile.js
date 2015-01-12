"use strict";

module.exports = function(grunt) {
  require("time-grunt")(grunt);
  require('load-grunt-tasks')(grunt, {
    pattern: [
      'grunt-*',
      '!grunt-configure'
    ]
  });

  grunt.initConfig(
    require("grunt-configure")("./etc/grunt/*.{js,json,yml}")
  );

  grunt.registerTask("build",   [ "psc:main", "dotPsci" ]);
  grunt.registerTask("test",    [ "psc:test", "execute:test" ]);
  grunt.registerTask("dev",     [ "build", "watch" ]);
  grunt.registerTask("default", [ "build" ]);
};
