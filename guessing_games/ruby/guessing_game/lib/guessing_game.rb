module GuessingGame
  def self.run
    puts "Guess the number!"
    puts "Please input your guess:"

    secret_number = rand(101)

    loop do
      guess = gets.to_i

      puts "You guessed: #{guess}"

      case guess <=> secret_number
      when -1 then puts "Too small!"
      when 1 then puts "Too big!"
      when 0 then
        puts "You win!"
        break
      end
    end
  end
end
