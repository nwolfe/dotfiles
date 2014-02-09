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
  Dir['lib/*.symlink'].each do |file|
    source = _cwd file
    dest = _home '.' + File.basename(file, '.symlink')
    _maybe_install source, dest
  end
  _maybe_install _cwd('vim'), _home('.vim')
  _maybe_install _cwd('bin'), _home('bin')
  _maybe_install _cwd('live-packs'), _home('.live-packs')
end

def _maybe_install(source, dest)
  unless @NO_TO_ALL
    puts # newline for readability
    if File.exist? dest
      ln_sf source, dest if _should_replace? dest
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

def _cwd(file)
  File.join ENV['PWD'], file
end

def _home(file)
  File.join ENV['HOME'], file
end

namespace :install do
  task :dotfiles do
    _install_dotfiles
  end

  task :submodules do
    _fetch_submodules
  end
end
