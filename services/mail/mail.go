package mail

import (
	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/matcornic/hermes"
	"gopkg.in/gomail.v2"
	"os"
)

var herm hermes.Hermes

func init() {
	herm = hermes.Hermes{
		// Optional Theme
		// Theme: new(Default)
		Product: hermes.Product{
			// Appears in header & footer of e-mails
			Name: "Databrary",
			Link: "https://nyu.databrary.org/",
			// Optional product logo
			Logo:      "https://github.com/databrary/assets/raw/master/logos/databrary/databrary-large.png", //TODO link to static
			Copyright: "Copyright © 2017 Databrary. All rights reserved.",
		},
	}
}

func SendPasswordRecovery(toName, toAddress, token string) error {
	conf := config.GetConf()
	link := fmt.Sprintf("%s://%s:%s/reset-password?token=%s",
		conf.GetString("address.scheme"),
		conf.GetString("address.domain"),
		conf.GetString("address.frontend_port"),
		token,
	)
	email := hermes.Email{
		Body: hermes.Body{
			Name: toName,
			Intros: []string{
				"You have received this email because a password reset request for your Databrary account was received.",
			},
			Actions: []hermes.Action{
				{
					Instructions: "Click the button below to reset your password:",
					Button: hermes.Button{
						Color: "#DC4D2F",
						Text:  "Reset your password",
						Link:  link,
					},
				},
			},
			Outros: []string{
				"If you did not request a password reset, please contact us immediately.",
			},
			Signature: "Love",
		},
	}
	body, err := herm.GenerateHTML(email)
	if err != nil {
		return err
	}
	err = SendEmail(body, "Password Reset", toAddress)
	return err
}

func SendPasswordRecoveryConfirmation(toName, toAddress string) error {
	email := hermes.Email{
		Body: hermes.Body{
			Name: toName,
			Intros: []string{
				"You have received this email to confirm your password has been reset.",
			},
			Outros: []string{
				"If you didn't perform this action please reset your password immediately",
				"and then contact us.",
			},
			Signature: "Love",
		},
	}
	body, err := herm.GenerateHTML(email)
	if err != nil {
		return err
	}
	err = SendEmail(body, "Password Reset Confirmation", toAddress)
	return err
}

func SendEmail(body, subject, toAddress string) error {
	m := gomail.NewMessage()
	m.SetHeader("From", "help@databrary.org")
	m.SetHeader("To", toAddress)
	m.SetHeader("Subject", subject)
	m.SetBody("text/html", body)

	d := gomail.NewDialer("smtp.gmail.com", 587, "maksim.levental@gmail.com", os.Getenv("GMAILPASSWORD"))

	err := d.DialAndSend(m)
	return err
}
