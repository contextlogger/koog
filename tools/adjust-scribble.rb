#!/usr/bin/env ruby

$dirname = ARGV.shift || raise
$srcfile = ARGV.shift || raise
$siteurl = ARGV.shift || raise

$mtime = File.mtime($srcfile)

def modify_file fn # &block
  data = File.read(fn)
  data = yield data
  File.open(fn, "w") do |output|
    output.write(data)
  end
end

class String
  def rm s
    sub(s, '')
  end

  def rm! s
    sub!(s, '')
  end
end

def postproc_html fn
  modify_file(fn) do |data|
    data = data.sub("</head>", '<style type="text/css" media="all">
.navi {
    background-color: #eeeeee;
    float: right;
    border: 1px solid #808080;
}
.navi td:hover {
    background-color: #aaaaaa;
}
</style><link rel="shortcut icon" href="http://www.hiit.fi/sites/all/themes/hiit/favicon.ico" type="image/x-icon" /></head>')
    data.sub!(%r{<div class="versionbox"><span class="version[\w]*">Version: [.\d]+</span></div>}, %q{<table class="navi"><tr><th>Go to</th></td><tr><td><a href="../index.html">Koog</a></td></tr><tr><td><a href="http://hiit.fi/uix">UIx</a></td></tr><tr><td><a href="http://hiit.fi">HIIT</a></td></tr></table>})
    data.rm!('<script type="text/javascript" src="scribble-common.js"></script>')
    data.rm!('<td style="width: 1em;"><a href="javascript:void(0);" title="Expand/Collapse" class="tocviewtoggle" onclick="TocviewToggle(this,&quot;tocview_0&quot;);">&#9658;</a></td>')
    data = data.sub("</body>", '<hr class="light"/><p><a href="%s">Koog</a> utility manual, by <a href="http://hiit.fi/u/hasu/">Tero Hasu</a>. Last modified %s.</p></body>' % [$siteurl, $mtime.to_s])
  end
end

def postproc_css fn
  modify_file(fn) do |data|
    data ## TODO -- remove fixed width styling for body text
  end
end

for fn in Dir[$dirname + '/*.html']
  postproc_html fn
end

for fn in Dir[$dirname + '/*.css']
  postproc_css fn
end
