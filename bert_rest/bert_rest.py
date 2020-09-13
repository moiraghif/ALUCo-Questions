import cherrypy
import transformers
import torch


# you can set everything model you wish, as long as TRANSFORMERS library
# understands what you want.
# Remember also to change the models in the BertServer class!
WORD_EMBEDDING_MODEL = "bert-base-multilingual-cased"
MAX_LEN = 512


def cos_sim(v1, v2):
    "Compute the cosine similarity between two vectors V1 and V2"
    return (torch.dot(v1, v2) / (v1.norm() * v2.norm())).item()


class BertServer:
    "The main class for BERT as REST service"

    @cherrypy.expose
    @cherrypy.tools.json_in()
    @cherrypy.tools.json_out()
    def cosine_similarity(self):
        "returns the cosine similarity of two sentences"
        # example of data passed in
        # {
        #     "substring": string_to_check,
        #     "sentence": a_sentence_to_contextualize,
        #     "candidates": [ a list of candidates ]
        # }
        # it returns data as follows:
        # {
        #     "a": score(a)
        #     "list": score(list)
        #     "of": score(of)
        #     "candidates": score(candidates)
        # }
        data = cherrypy.request.json
        # the sentence is extracted from the json
        sentence = data["substring"] + " " + data["sentence"]
        sentence_encoded = self.get_encoding(
            torch.tensor([self.get_tokens(sentence)[0]]))[0, :]
        # data is grouped by length
        grouped_data = dict()
        for sent in data["candidates"]:
            t, l = self.get_tokens(sent + " " + data["sentence"])
            if l not in grouped_data.keys():
                grouped_data[l] = list()
            grouped_data[l].append((sent, t))
        # then data is passed to the NN in batch according to their size
        del data
        out = dict()
        for k, v in grouped_data.items():
            matrix = torch.tensor(list(map(lambda e: e[1],
                                           v)))
            embeddings = self.get_encoding(matrix)
            del matrix
            for i, (sent, _) in enumerate(v):
                out[sent] = cos_sim(embeddings[i, :],
                                    sentence_encoded)
            del embeddings
        del sentence_encoded
        del grouped_data
        return out

    def get_encoding(self, tokens):
        "returns the encoding of the SENTENCE"
        embeddings, logits = self.bert(tokens)
        # the embedding of the sentence is [CLS] token (101)
        # that is in position 0 [:, 0, :]
        return embeddings[:, 0, :]

    def get_tokens(self, sent):
        "returns the list of tokens for a SENT and its length"
        tokens = self.tokenizer.encode(sent)
        if len(tokens) >= MAX_LEN:
            tokens = tokens[0:(MAX_LEN - 1)]
        return (tokens, len(tokens))

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
