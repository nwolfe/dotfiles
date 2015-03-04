require 'rake'

@YES_TO_ALL = false
@NO_TO_ALL = false

task :install do
  _install_dotfiles
  _install_binaries
  _install_vim
  _install_lein_profile
  # _install_ohmyzsh_theme
  # _install_emacs_live
end

def _install_dotfiles()
  Dir['lib/*'].each do |file|
    source = _cwd file
    dest = _home '.' + File.basename(file)
    _maybe_install source, dest
  end
end

def _install_binaries()
  _maybe_install _cwd('bin'), _home('bin')
end

def _install_vim()
  _maybe_install _cwd('vim'), _home('.vim')
end

def _install_emacs_live()
  _maybe_install _cwd('live-packs'), _home('.live-packs')
end

def _install_lein_profile()
  source = File.join ENV['PWD'], 'misc', 'profiles.clj'
  destination = File.join ENV['HOME'], '.lein', 'profiles.clj'
  mkdir_p _home('.lein')
  _maybe_install source, destination
end

def _install_ohmyzsh_theme()
  theme = 'goodsam.zsh-theme'
  oh_my_zsh_themes = File.join ENV['HOME'], '.oh-my-zsh', 'themes'
  if File.exist? oh_my_zsh_themes
    source = File.join ENV['PWD'], 'misc', theme
    dest = File.join oh_my_zsh_themes, theme
    _maybe_install source, dest
  end
end

def _maybe_install(source, dest)
  unless @NO_TO_ALL
    puts # newline for readability
    if File.exist? dest
      ln_sf source, dest if _should_replace? dest
    else
      # Use the force option to overwrite dead symlinks.
      # File.exist? will return false if the symlink is dead, but
      # ln_s will fail because it thinks the symlink is a file
      ln_sf source, dest
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
