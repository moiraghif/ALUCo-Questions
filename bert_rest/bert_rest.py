import cherrypy
import transformers
import torch


# you can set everything model you wish, as long as TRANSFORMERS library
# understands what you want.
# Remember also to change the models in the BertServer class!
WORD_EMBEDDING_MODEL = "bert-base-multilingual-cased"


def cos_sim(v1, v2):
    "Compute the cosine similarity between two vectors V1 and V2"
    return (torch.dot(v1, v2) / (v1.norm() * v2.norm())).item()


class BertServer:
    "The main class for BERT as REST service"

    @cherrypy.expose
    @cherrypy.tools.json_in()
    def cosine_similarity(self):
        "returns the cosine similarity of two sentences"
        # example of data passed in
        # {
        #     "sent1": sentence_number_1,
        #     "sent2": sentence_number_2
        # }
        data = cherrypy.request.json
        v1 = self.get_encoding(data["sent1"])
        v2 = self.get_encoding(data["sent2"])
        return str(cos_sim(v1, v2))

    def get_encoding(self, sentence: str):
        "returns the encoding of the SENTENCE"
        tokens = torch.tensor([self.tokenizer.encode(sentence)])
        embeddings, logits = self.bert(tokens)
        # the embedding of the sentence is [CLS] token (101)
        # that is in position 0 [:, 0, :]
        # since the batch size is = 1, it takes only the first element [0, :, :]
        # So, it takes the first element of the only sentence passed through NN
        return embeddings[0, 0, :]

    def __init__(self):
        # https://www.aclweb.org/anthology/S17-2001/
        # https://arxiv.org/pdf/1810.04805.pdf
        self.tokenizer = transformers.BertTokenizer \
                                     .from_pretrained(WORD_EMBEDDING_MODEL)
        self.bert = transformers.BertModel \
                                .from_pretrained(WORD_EMBEDDING_MODEL)


if __name__ == "__main__":
    config = {"server.socket_host": "0.0.0.0"}  # on localhost
    cherrypy.config.update(config)
    cherrypy.quickstart(BertServer())
