
# Validate email address format
is_valid_email <- function(email) {
  grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)
}

# Generate and send OTP (simulated for now)
send_otp <- function(email) {
  otp <- sprintf("%06d", sample(0:999999, 1))  # Generate random 6-digit OTP
  message(sprintf("OTP sent to %s: %s", email, otp))  # Simulate sending email
  return(otp)
}

# Verify OTP
verify_otp <- function(entered_otp, generated_otp) {
  entered_otp == generated_otp
}
