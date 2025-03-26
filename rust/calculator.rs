use std::collections::HashMap;
use std::io::{self, Write};
use std::error::Error;
use std::fmt;

#[derive(Debug)]
enum CalculatorError {
    ParseError(String),
    UndefinedVariable(String),
    DivisionByZero,
    InvalidExpression(String),
    IOError(io::Error),
}

impl fmt::Display for CalculatorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CalculatorError::ParseError(msg) => write!(f, "Ayrıştırma hatası: {}", msg),
            CalculatorError::UndefinedVariable(name) => write!(f, "Tanımlanmamış değişken: {}", name),
            CalculatorError::DivisionByZero => write!(f, "Sıfıra bölme hatası"),
            CalculatorError::InvalidExpression(msg) => write!(f, "Geçersiz ifade: {}", msg),
            CalculatorError::IOError(e) => write!(f, "Giriş/Çıkış hatası: {}", e),
        }
    }
}

impl Error for CalculatorError {}

impl From<io::Error> for CalculatorError {
    fn from(err: io::Error) -> CalculatorError {
        CalculatorError::IOError(err)
    }
}

#[derive(Debug, Clone)]
enum Token {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    Identifier(String),
    Print,
    LeftParen,
    RightParen,
}

struct Calculator {
    variables: HashMap<String, f64>,
}

impl Calculator {
    fn new() -> Self {
        Calculator {
            variables: HashMap::new(),
        }
    }

    fn evaluate(&mut self, expression: &str) -> Result<f64, CalculatorError> {
        let tokens = self.tokenize(expression)?;
        self.parse_expression(&tokens, 0).map(|(val, _)| val)
    }

    fn tokenize(&self, input: &str) -> Result<Vec<Token>, CalculatorError> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&c) = chars.peek() {
            match c {
                ' ' | '\t' | '\n' => {
                    chars.next();
                    continue;
                }
                '+' => {
                    tokens.push(Token::Plus);
                    chars.next();
                }
                '-' => {
                    tokens.push(Token::Minus);
                    chars.next();
                }
                '*' => {
                    tokens.push(Token::Multiply);
                    chars.next();
                }
                '/' => {
                    tokens.push(Token::Divide);
                    chars.next();
                }
                '=' => {
                    tokens.push(Token::Assign);
                    chars.next();
                }
                '(' => {
                    tokens.push(Token::LeftParen);
                    chars.next();
                }
                ')' => {
                    tokens.push(Token::RightParen);
                    chars.next();
                }
                c if c.is_alphabetic() => {
                    let mut identifier = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() {
                            identifier.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if identifier == "print" {
                        tokens.push(Token::Print);
                    } else {
                        tokens.push(Token::Identifier(identifier));
                    }
                }
                c if c.is_digit(10) || c == '.' => {
                    let mut number = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_digit(10) || c == '.' {
                            number.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    match number.parse::<f64>() {
                        Ok(n) => tokens.push(Token::Number(n)),
                        Err(_) => return Err(CalculatorError::ParseError("Geçersiz sayı formatı".to_string())),
                    }
                }
                _ => return Err(CalculatorError::ParseError(format!("Geçersiz karakter: {}", c))),
            }
        }
        Ok(tokens)
    }

    // Yeni recursive descent parsing fonksiyonları
    fn parse_expression(&mut self, tokens: &[Token], pos: usize) -> Result<(f64, usize), CalculatorError> {
        if tokens.is_empty() {
            return Err(CalculatorError::InvalidExpression("Boş ifade".to_string()));
        }

        let (mut left, mut new_pos) = self.parse_term(tokens, pos)?;

        while new_pos < tokens.len() {
            match &tokens[new_pos] {
                Token::Plus => {
                    let (right, right_pos) = self.parse_term(tokens, new_pos + 1)?;
                    left += right;
                    new_pos = right_pos;
                },
                Token::Minus => {
                    let (right, right_pos) = self.parse_term(tokens, new_pos + 1)?;
                    left -= right;
                    new_pos = right_pos;
                },
                _ => break,
            }
        }

        Ok((left, new_pos))
    }

    fn parse_term(&mut self, tokens: &[Token], pos: usize) -> Result<(f64, usize), CalculatorError> {
        if pos >= tokens.len() {
            return Err(CalculatorError::InvalidExpression("Beklenmeyen ifade sonu".to_string()));
        }

        let (mut left, mut new_pos) = self.parse_factor(tokens, pos)?;

        while new_pos < tokens.len() {
            match &tokens[new_pos] {
                Token::Multiply => {
                    let (right, right_pos) = self.parse_factor(tokens, new_pos + 1)?;
                    left *= right;
                    new_pos = right_pos;
                },
                Token::Divide => {
                    let (right, right_pos) = self.parse_factor(tokens, new_pos + 1)?;
                    if right == 0.0 {
                        return Err(CalculatorError::DivisionByZero);
                    }
                    left /= right;
                    new_pos = right_pos;
                },
                _ => break,
            }
        }

        Ok((left, new_pos))
    }

    fn parse_factor(&mut self, tokens: &[Token], pos: usize) -> Result<(f64, usize), CalculatorError> {
        if pos >= tokens.len() {
            return Err(CalculatorError::InvalidExpression("Beklenmeyen ifade sonu".to_string()));
        }

        match &tokens[pos] {
            Token::Number(n) => Ok((*n, pos + 1)),
            Token::Identifier(name) => {
                // İşlem atama ise
                if pos + 1 < tokens.len() && matches!(tokens[pos + 1], Token::Assign) {
                    let (value, new_pos) = self.parse_expression(tokens, pos + 2)?;
                    self.variables.insert(name.clone(), value);
                    Ok((value, new_pos))
                } else if let Some(&value) = self.variables.get(name) {
                    Ok((value, pos + 1))
                } else {
                    Err(CalculatorError::UndefinedVariable(name.clone()))
                }
            },
            Token::Print => {
                if pos + 1 >= tokens.len() {
                    return Err(CalculatorError::InvalidExpression("Print ifadesi için değer gerekli".to_string()));
                }
                let (value, new_pos) = self.parse_expression(tokens, pos + 1)?;
                println!("{}", value);
                Ok((value, new_pos))
            },
            Token::LeftParen => {
                let (value, new_pos) = self.parse_expression(tokens, pos + 1)?;
                if new_pos >= tokens.len() || !matches!(tokens[new_pos], Token::RightParen) {
                    return Err(CalculatorError::InvalidExpression("Kapanmamış parantez".to_string()));
                }
                Ok((value, new_pos + 1))
            },
            _ => Err(CalculatorError::InvalidExpression("Geçersiz ifade başlangıcı".to_string())),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut calculator = Calculator::new();
    println!("Rust Hesap Makinesi");
    println!("Çıkmak için 'exit' yazın");

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        if input == "exit" {
            break;
        }

        match calculator.evaluate(input) {
            Ok(result) => println!("{}", result),
            Err(e) => println!("Hata: {}", e),
        }
    }

    Ok(())
}
