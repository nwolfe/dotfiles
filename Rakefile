require 'rake'

SKIP = %w[Rakefile README.md osx sources]

task :install do
  _fetch_submodules
  Dir['*'].each do |file|
    next if SKIP.include? file

    if File.exist? File.join(ENV['HOME'], ".#{file}")
      _maybe_replace file
    else
      _install(file)
    end
  end
end

def _install(file)
  puts "Linking ~/.#{file}"
  source = File.join(ENV['PWD'], file)
  target = File.join(ENV['HOME'], '.' + file)
  `ln -s "#{source}" "#{target}"`
end

def _maybe_replace(file)
  print "overwrite ~/.#{file}? [yn] "

  case $stdin.gets.chomp
  when 'y'
    `rm "#{File.join(ENV['HOME'], '.' + file)}"`
    _install file
  else
    puts "skipping ~/.#{file}"
  end
end

def _fetch_submodules()
  `git submodule init`
  `git submodule update`
end
