ProcessCompletionText = function()
{
  
  send.mail(
    
    from = 'email@someemailserver', # The e-mail address from which the text will be sent
    to = '12345678901@emailtoSMSGateway', # The phone number where the text is to be sent.  A page which contains a list of email to SMS gateways may be found here: https://github.com/mfitzp/List_of_SMS_gateways/blob/master/email2sms.csv
    subject = 'The Process Is Complete',
    body = 'Return to the terminal',
    smtp = list(host.name = 'yourhost.name', port = 123, user.name = 'youruser.name', passwd = 'yourpassword', ssl = TRUE),
    authenticate = TRUE,
    send = TRUE
    
  )
  
}
