#!/usr/bin/perl
use strict;
use warnings;

package Calculator::Error;
use base 'Error::Simple';

package Calculator;

sub new {
    my ($class) = @_;
    my $self = {
        variables => {}
    };
    bless $self, $class;
    return $self;
}

sub tokenize {
    my ($self, $input) = @_;
    my @tokens;
    my $pos = 0;
    my $length = length($input);
    
    while ($pos < $length) {
        my $char = substr($input, $pos, 1);
        
        if ($char =~ /\s/) {
            $pos++;
            next;
        }
        
        if ($char =~ /[+\-*\/()=]/) {
            push @tokens, $char;
            $pos++;
            next;
        }
        
        if ($char =~ /[a-zA-Z_]/) {
            my $identifier = '';
            while ($pos < $length && substr($input, $pos, 1) =~ /[a-zA-Z0-9_]/) {
                $identifier .= substr($input, $pos, 1);
                $pos++;
            }
            push @tokens, $identifier;
            next;
        }
        
        if ($char =~ /[0-9]/) {
            my $number = '';
            while ($pos < $length && substr($input, $pos, 1) =~ /[0-9.]/) {
                $number .= substr($input, $pos, 1);
                $pos++;
            }
            if ($number =~ /^\d+\.?\d*$/) {
                push @tokens, $number;
            } else {
                Calculator::Error->throw("Geçersiz sayı formatı: $number");
            }
            next;
        }
        
        Calculator::Error->throw("Geçersiz karakter: $char");
    }
    
    return \@tokens;
}

sub evaluate {
    my ($self, $tokens) = @_;
    return 0 unless @$tokens;
    
    my $token = $tokens->[0];
    
    if ($token eq 'print') {
        if (@$tokens < 2) {
            Calculator::Error->throw("Print ifadesi için değer eksik");
        }
        my $var_name = $tokens->[1];
        if (!exists $self->{variables}{$var_name}) {
            Calculator::Error->throw("Tanımlanmamış değişken: $var_name");
        }
        print "$var_name = " . $self->{variables}{$var_name} . "\n";
        return $self->{variables}{$var_name};
    }
    
    if ($token =~ /^[a-zA-Z_][a-zA-Z0-9_]*$/) {
        if (@$tokens >= 3 && $tokens->[1] eq '=') {
            my $var_name = $token;
            my $expr_tokens = [@$tokens[2..$#$tokens]];
            my $value = $self->evaluate_expression($expr_tokens);
            $self->{variables}{$var_name} = $value;
            return $value;
        }
        if (exists $self->{variables}{$token}) {
            return $self->{variables}{$token};
        }
        Calculator::Error->throw("Tanımlanmamış değişken: $token");
    }
    
    return $self->evaluate_expression($tokens);
}

sub parse_expression {
    my ($self, $tokens, $pos) = @_;
    return $self->parse_add_sub($tokens, $pos);
}

sub parse_add_sub {
    my ($self, $tokens, $pos) = @_;
    
    my ($left, $new_pos) = $self->parse_mul_div($tokens, $pos);
    
    while ($new_pos < @$tokens && ($tokens->[$new_pos] eq '+' || $tokens->[$new_pos] eq '-')) {
        my $op = $tokens->[$new_pos];
        $new_pos++;
        
        my ($right, $next_pos) = $self->parse_mul_div($tokens, $new_pos);
        
        if ($op eq '+') {
            $left += $right;
        } else {
            $left -= $right;
        }
        
        $new_pos = $next_pos;
    }
    
    return ($left, $new_pos);
}

sub parse_mul_div {
    my ($self, $tokens, $pos) = @_;
    
    my ($left, $new_pos) = $self->parse_primary($tokens, $pos);
    
    while ($new_pos < @$tokens && ($tokens->[$new_pos] eq '*' || $tokens->[$new_pos] eq '/')) {
        my $op = $tokens->[$new_pos];
        $new_pos++;
        
        my ($right, $next_pos) = $self->parse_primary($tokens, $new_pos);
        
        if ($op eq '*') {
            $left *= $right;
        } else {
            if ($right == 0) {
                Calculator::Error->throw("Sıfıra bölme hatası");
            }
            $left /= $right;
        }
        
        $new_pos = $next_pos;
    }
    
    return ($left, $new_pos);
}

sub parse_primary {
    my ($self, $tokens, $pos) = @_;
    
    my $token = $tokens->[$pos];
    
    if ($token =~ /^\d+\.?\d*$/) {
        return ($token, $pos + 1);
    }
    
    if ($token =~ /^[a-zA-Z_][a-zA-Z0-9_]*$/) {
        if (!exists $self->{variables}{$token}) {
            Calculator::Error->throw("Tanımlanmamış değişken: $token");
        }
        return ($self->{variables}{$token}, $pos + 1);
    }
    
    if ($token eq '(') {
        $pos++;
        my ($expr, $new_pos) = $self->parse_expression($tokens, $pos);
        
        if ($new_pos >= @$tokens || $tokens->[$new_pos] ne ')') {
            Calculator::Error->throw("Parantez kapatılmamış");
        }
        
        return ($expr, $new_pos + 1);
    }
    
    Calculator::Error->throw("Geçersiz token: $token");
}

# İşlem önceliği
sub evaluate_expression {
    my ($self, $tokens) = @_;
    return 0 unless @$tokens;
    
    my ($result, $pos) = $self->parse_expression($tokens, 0);
    
    if ($pos < @$tokens) {
        Calculator::Error->throw("İfadenin sonunda beklenmeyen token: " . $tokens->[$pos]);
    }
    
    return $result;
}

package main;

my $calculator = Calculator->new();

while (1) {
    print "> ";
    my $input = <STDIN>;
    last unless defined $input;
    chomp $input;
    
    next if $input eq '';
    
    eval {
        my $tokens = $calculator->tokenize($input);
        my $result = $calculator->evaluate($tokens);
        print "$result\n" unless $input =~ /^print/;
    };
    
    if ($@) {
        if ($@->isa('Calculator::Error')) {
            print "Hata: " . $@->text() . "\n";
        } else {
            print "Beklenmeyen hata: $@\n";
        }
    }
} 