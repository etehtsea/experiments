module DiningPhilosophers
  FORKS = Array.new(5) { Mutex.new }

  class Philosopher
    def initialize(name, seat, times_to_eat = 10)
      @name = name
      @forks = [seat % 5, (seat + 1) % 5].sort
      puts "#{@name} forks: #{@forks}"
      @times_to_eat = times_to_eat
    end

    def revive
      @thread = Thread.new do
        loop do
          pick_forks
          @times_to_eat > 0 ? eat : go_away
          put_forks
          think
        end
      end
    end

    private

    def pick_forks
      FORKS[@forks.first].lock
      sleep rand
      FORKS[@forks.last].lock
      puts "#{@name} picked forks #{@forks}"
    end

    def put_forks
      FORKS.values_at(*@forks).each(&:unlock)
      puts "#{@name} put forks #{@forks}"
    end

    def eat
      sleep rand
      @times_to_eat -= 1
      puts "--------- #{@name} #{@times_to_eat} times to eat left"
    end

    def think
      puts "#{@name} is thinking!"
      sleep rand
    end

    def go_away
      puts ">=======> #{@name} stopped!"
      @thread.exit
    end
  end

  PHILOSOPHERS_AMOUNT = 25

  def self.run
    Thread.abort_on_exception = true

    Array.new(PHILOSOPHERS_AMOUNT) do |i|
      Philosopher.new("PHIL#{i}", i, rand(30)).revive
    end.each(&:join)
  end
end
