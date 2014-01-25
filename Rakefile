require 'rake'

# TODO: Add support for Y/N to all

task :install do
  _fetch_submodules
  _install_dotfiles
end

def _fetch_submodules()
  `git submodule init`
  `git submodule update`
end

def _install_dotfiles()
  ignore = %w[Rakefile README.md osx sources]
  install = Dir['*'].reject { |file| ignore.include? file }
  install.each do |file|
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
    ln_s source, dest
  end
end

def _maybe_replace(source, dest)
  print "overwrite #{dest}? [yn] "
  case $stdin.gets.chomp
  when 'y'
    if File.directory? dest
      rm_r dest
    else
      rm dest
    end
    ln_s source, dest
  when 'Y'
  end
end
