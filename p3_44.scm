#lang racket

;I think Ben is right and Louis is wrong. For transfer, you only need to handle the relavent
;accounts once (by either withdraw/deposit). For exchange, you need to interact with the relavent
;accounts several times (check balance => calculate how much money you need to withdraw/deposit
;=> withdraw/deposit), so concurrency between different accounts matters.