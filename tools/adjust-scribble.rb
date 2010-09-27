#!/usr/bin/env ruby

dirname = ARGV.shift || raise

srcfile = 'manual.scrbl'
$mtime = File.mtime(srcfile)

def modify_file fn # &block
  data = File.read(fn)
  data = yield data
  File.open(fn, "w") do |output|
    output.write(data)
  end
end

def postproc_html fn
  modify_file(fn) do |data|
    data = data.sub(/<div class="versionbox"><span class="version[\w]*">Version: [.\d]+<\/span><\/div>/, '')
    data = data.sub("</body>", '<hr class="light"/><p><a href="http://koog.contextlogger.org/">Koog</a> utility manual, by <a href="http://hiit.fi/u/hasu/">Tero Hasu</a>. Last modified %s.</p></body>' % $mtime.to_s)
    data = data.sub("</head>", '<link rel="shortcut icon" href="http://www.hiit.fi/sites/all/themes/hiit/favicon.ico" type="image/x-icon" /></head>')
  end
end

def postproc_css fn
  modify_file(fn) do |data|
    data
  end
end

for fn in Dir[dirname + '/*.html']
  postproc_html fn
end

for fn in Dir[dirname + '/*.css']
  postproc_css fn
end
