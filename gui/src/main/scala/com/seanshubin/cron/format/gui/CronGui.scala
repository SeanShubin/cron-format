package com.seanshubin.cron.format.gui

import scala.swing
import swing._
import scala.swing.event
import event.ValueChanged
import java.awt.Dimension
import swing.Color
import javax.swing.JPanel
import com.seanshubin.cron.format.domain.CronFormat


object CronGui extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Readable Cron"
    contents = new BorderPanel {
      add(fields, BorderPanel.Position.Center)
      add(actions, BorderPanel.Position.South)
    }
    size = new Dimension(800, 400)
  }

  private var inEvent: Boolean = false

  val documentationText = """round trip conversion of cron expressions to a more readable format
                            |ranges supported
                            |    number (12)
                            |    range (10-20)
                            |    increment (1/5)
                            |    multiple (1,2,3)
                            |    all (*)
                            |not supported
                            |    mixing types of ranges (for example, both increment and range)
                            |    the day of week field (sixth field, set it to ?)
                            |    names for month (jan, feb)
                            |    names for day of week (sun, mon)
                            | """.stripMargin

  val cronExpression = new TextField {
    text = CronFormat.cronDefault
    columns = 20
  }

  val verboseExpression = new TextField {
    columns = 20
  }

  val errorMessage = new TextField {
    columns = 20
  }

  val helpText = new TextArea {
    columns = 30
    rows = 12
    text = documentationText
    enabled = false
    background = new JPanel().getBackground
  }
  helpText.peer.setDisabledTextColor(new Color(0, 0, 0))

  val fields = new GridBagPanel {
    private def addHelp(row: Int, component: Component) {
      val constraints = new Constraints

      constraints.insets = new Insets(10, 10, 10, 10)
      constraints.fill = GridBagPanel.Fill.Both

      constraints.gridy = row
      constraints.gridx = 0
      constraints.gridwidth = 2
      layout(component) = constraints
    }

    private def addField(row: Int, caption: String, component: Component) {
      val constraints = new Constraints

      constraints.insets = new Insets(5, 5, 5, 5)
      constraints.anchor = GridBagPanel.Anchor.West
      constraints.fill = GridBagPanel.Fill.Horizontal

      constraints.gridy = row
      constraints.gridx = 0
      constraints.weightx = 0.0
      layout(new Label(caption)) = constraints

      constraints.gridx = 1
      constraints.weightx = 1.0
      layout(component) = constraints
    }

    addHelp(0, helpText)
    addField(1, "Cron", cronExpression)
    addField(2, "Verbose", verboseExpression)
    addField(3, "Error", errorMessage)
  }

  val exitButton = new Button {
    action = Action("Exit") {
      quit()
    }
  }

  val actions = new FlowPanel(FlowPanel.Alignment.Right)(exitButton)

  listenTo(cronExpression, verboseExpression)

  reactions += {
    case ValueChanged(`cronExpression`) => cronExpressionUpdated()
    case ValueChanged(`verboseExpression`) => verboseExpressionUpdated()
  }

  cronExpressionUpdated()

  private def cronExpressionUpdated() {
    if (!inEvent) {
      inEvent = true
      try {
        val verboseText: String = CronFormat.cronToVerbose(cronExpression.text)
        verboseExpression.text = verboseText
        errorMessage.text = ""
      }
      catch {
        case ex: Exception => {
          verboseExpression.text = ""
          errorMessage.text = ex.getMessage
        }
      }
      inEvent = false
    }
  }

  private def verboseExpressionUpdated() {
    if (!inEvent) {
      inEvent = true
      try {
        val cronText: String = CronFormat.verboseToCron(verboseExpression.text)
        cronExpression.text = cronText
        errorMessage.text = ""
      }
      catch {
        case ex: Exception => {
          cronExpression.text = ""
          errorMessage.text = ex.getMessage
        }
      }
      inEvent = false
    }
  }
}
