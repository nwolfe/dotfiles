require 'rake'

# TODO: Add support for Y/N to all
# TODO: Test _maybe_replace on a dest directory (not a link)

task :install do
  _fetch_submodules
  _install_dotfiles
end

def _fetch_submodules()
  `git submodule init`
  `git submodule update`
end

def _install_dotfiles()
  Dir['*'].each do |file|
    next if %w[Rakefile README.md osx sources].include? file
    puts # newline for readability
    source = File.join ENV['PWD'], file
    dest = File.join ENV['HOME'], '.' + file
    _maybe_install source, dest
  end
end

def _maybe_install(source, dest)
  if File.exist? dest
    _maybe_replace source, dest
  else
    _install source, dest
  end
end

def _maybe_replace(source, dest)
  print "overwrite #{dest}? [yn] "

  case $stdin.gets.chomp
  when 'y'
    rm dest # TODO: This might fail when dest is a directory
    _install source, dest
  end
end

def _install(source, dest)
  ln_s source, dest
end
