#lang racket

;I don't agree with Ben, because check balance never "set!" the balance.
;The result for "(dispatch 'balance)" may be not that up-to-date, but it will never cause anomalous behaviors.