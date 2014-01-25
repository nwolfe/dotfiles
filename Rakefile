require 'rake'

@YES_TO_ALL = false
@NO_TO_ALL = false

task :install do
  _fetch_submodules
  _install_dotfiles
end

def _fetch_submodules()
  `git submodule init`
  `git submodule update`
end

def _install_dotfiles()
  ignore = %w[Rakefile README.md osx sources notes]
  install = Dir['*'].reject { |file| ignore.include? file }
  install.each do |file|
    source = File.join ENV['PWD'], file
    dest = File.join ENV['HOME'], '.' + file
    _maybe_install source, dest
  end
end

def _maybe_install(source, dest)
  unless @NO_TO_ALL
    puts # newline for readability
    if File.exist? dest
      _replace source, dest if _should_replace? dest
    else
      ln_s source, dest
    end
  end
end

def _should_replace?(dest)
  return true if @YES_TO_ALL
  print "overwrite #{dest}? [ynYN] "
  choice = STDIN.gets.chomp
  @YES_TO_ALL = (choice == 'Y')
  @NO_TO_ALL = (choice == 'N')
  return choice.downcase == 'y'
end

def _replace(source, dest)
  if File.directory? dest
    rm_r dest
  else
    rm dest
  end
  ln_s source, dest
end
